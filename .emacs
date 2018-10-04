(add-to-list 'load-path "~/.emacs.d/lisp/")
(add-to-list 'exec-path "/usr/local/bin/")

(setq current-language-environment "UTF-8")
(setq default-input-method "chinese-py")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(autoload 'dired-jump "dired-x"
"Jump to Dired buffer corresponding to current buffer." t)

(fset 'yes-or-no-p 'y-or-n-p)
(iswitchb-mode 1)
(global-set-key "\C-x\C-j" 'dired-jump)
(global-set-key [f11] 'compile)
(global-set-key "\M-p" 'goto-line)
(setq bookmark-save-flag 1)
(global-set-key "\C-x\M-i" 'bs-show)
(global-font-lock-mode t)
(column-number-mode t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default js2-basic-offset 2)
(setq-default c-basic-offset 2)
(setq indent-line-function 'insert-tab)

(setq make-backup-files nil)

(define-key global-map (kbd "RET") 'newline-and-indent)
(define-key global-map (kbd "C-j") 'newline-and-indent)
;; Set cursor and mouse-pointer colours
;(set-cursor-color "green")
;(set-mouse-color "green")

;; Set region background colour
(set-face-background 'region "blue")

;; Set emacs background colour
(set-background-color "black")

(set-foreground-color "white")
; (tool-bar-mode -1)

(setq js-indent-level 2)

(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
(js-mode "]*>" "")
(css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
(not (gnutls-available-p))))
(proto (if no-ssl "http" "https")))
;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
(add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
(when (< emacs-major-version 24)
;; For important compatibility libraries like cl-lib
(add-to-list 'package-archives '("gnu" . (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)

;; auto completion
(require 'company)
(global-company-mode)    

;; helm
(require 'helm)
(require 'helm-config)
(helm-mode 1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (company robe helm-robe helm-dash ruby-electric helm-ag helm-projectile helm js2-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.
(global-set-key (kbd "C-c h") 'helm-command-prefix)
(global-set-key (kbd "C-x C-f") 'helm-find-files)
(global-unset-key (kbd "C-x c"))

(define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB work in terminal
(define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z

(when (executable-find "curl")
  (setq helm-google-suggest-use-curl-p t))

(setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
      helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
      helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
      helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
      helm-ff-file-name-history-use-recentf t
      helm-echo-input-in-header-line t)

(defun spacemacs//helm-hide-minibuffer-maybe ()
  "Hide minibuffer in Helm session if we use the header line as input field."
  (when (with-helm-buffer helm-echo-input-in-header-line)
    (let ((ov (make-overlay (point-min) (point-max) nil nil t)))
      (overlay-put ov 'window (selected-window))
      (overlay-put ov 'face
                   (let ((bg-color (face-background 'default nil)))
                     `(:background ,bg-color :foreground ,bg-color)))
      (setq-local cursor-type nil))))


(add-hook 'helm-minibuffer-set-up-hook
          'spacemacs//helm-hide-minibuffer-maybe)

(setq helm-autoresize-max-height 0)
(setq helm-autoresize-min-height 20)
(helm-autoresize-mode 1)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-global-mode)
(setq projectile-completion-system 'helm)
(helm-projectile-on)

;; javascript
(require 'js2-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

;; Ruby
(add-hook 'ruby-mode-hook 'ruby-electric-mode)
(add-hook 'ruby-mode-hook 'robe-mode)
(eval-after-load 'company
  '(push 'company-robe company-backends))
