;; ------------
;; Global conf
;; ------------
(add-to-list 'load-path "~/.emacs.d/lisp/")

(setq current-language-environment "UTF-8")
(setq default-input-method "chinese-py")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-face-attribute 'default nil
                    :height 125 :weight 'normal)

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

(setq gc-cons-threshold (eval-when-compile (* 1024 1024 1024)))
(run-with-idle-timer 2 t (lambda () (garbage-collect)))

;; jump back and forward shortcut keys.
(defun marker-is-point-p (marker)
  "test if marker is current point"
  (and (eq (marker-buffer marker) (current-buffer))
       (= (marker-position marker) (point))))

(defun push-mark-maybe ()
  "push mark onto `global-mark-ring' if mark head or tail is not current location"
  (if (not global-mark-ring) (error "global-mark-ring empty")
    (unless (or (marker-is-point-p (car global-mark-ring))
                (marker-is-point-p (car (reverse global-mark-ring))))
      (push-mark))))

(defun backward-global-mark ()
  "use `pop-global-mark', pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark))

(defun forward-global-mark ()
  "hack `pop-global-mark' to go in reverse, pushing current point if not on ring."
  (interactive)
  (push-mark-maybe)
  (setq global-mark-ring (nreverse global-mark-ring))
  (when (marker-is-point-p (car global-mark-ring))
    (call-interactively 'pop-global-mark))
  (call-interactively 'pop-global-mark)
  (setq global-mark-ring (nreverse global-mark-ring)))

(global-set-key [M-s-left] (quote backward-global-mark))
(global-set-key [M-s-right] (quote forward-global-mark))

(defun nuke_traling ()
  (add-hook 'before-save-hook #'delete-trailing-whitespace nil t))
(add-hook 'prog-mode-hook #'nuke_traling)

;;
;; INSTALL PACKAGES
;; --------------------------------------
;;

(require 'package)
(setq package-archives '(("gnu" . "http://mirrors.163.com/elpa/gnu/")
                         ("melpa" . "https://melpa.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(ein
    ;;elpy
    jedi
    flycheck
    ;;material-theme
    yasnippet
    yasnippet-snippets
    py-autopep8
    multi-web-mode
    js2-mode
    js2-refactor
    xref-js2
    company
    company-tern
    magit
    flymake
    lsp-mode
    company-lsp
    lsp-ui
    lsp-java
    helm
    helm-lsp
    use-package
    projectile
    scala-mode
    sbt-mode
    helm-projectile
    dockerfile-mode
    autopair
    exec-path-from-shell))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)

;; BASIC CUSTOMIZATION
;; --------------------------------------

(setq inhibit-startup-message t) ;; hide the startup message
;;(load-theme 'material t) ;; load material theme
(global-linum-mode t) ;; enable line numbers globally

;; PYTHON CONFIGURATION
;; --------------------------------------

;; use flycheck not flymake with elpy
;(when (require 'flycheck nil t)
;  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;  (add-hook 'elpy-mode-hook 'flycheck-mode))
'(flycheck-check-syntax-automatically (quote (save idle-change mode-enabled)))
'(flycheck-idle-change-delay 1)

;; enable autopep8 formatting on save
;(require 'py-autopep8)
;(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)

(require 'multi-web-mode)
(setq mweb-default-major-mode 'html-mode)
(setq mweb-tags '((php-mode "<\\?php\\|<\\? \\|<\\?=" "\\?>")
                  (js-mode "<script +\\(type=\"text/javascript\"\\|language=\"javascript\"\\)[^>]*>" "</script>")
                  (css-mode "<style +type=\"text/css\"[^>]*>" "</style>")))
(setq mweb-filename-extensions '("php" "htm" "html" "ctp" "phtml" "php4" "php5"))
(multi-web-global-mode 1)

;; javascript
(require 'js2-mode)
(add-hook 'js2-mode-hook 'ac-js2-mode)
(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . js2-jsx-mode))
(add-to-list 'interpreter-mode-alist '("node" . js2-jsx-mode))

(require 'js2-refactor)
(require 'xref-js2)
(add-hook 'js2-mode-hook #'js2-refactor-mode)
(js2r-add-keybindings-with-prefix "C-c C-r")
(define-key js-mode-map (kbd "M-.") nil)
(add-hook 'js2-mode-hook (lambda ()
                           (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))

(require 'company)
(require 'company-tern)
(add-to-list 'company-backends 'company-tern)
(add-hook 'js2-mode-hook (lambda ()
                           (tern-mode)
                           (company-mode)))

;; ;; misc
;; ; have the same path conf from shell
(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;(custom-set-variables
;;  ;; custom-set-variables was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;; '(package-selected-packages
;;   (quote
;;    (dockerfile-mode docker markdown-preview-mode markdown-mode yaml-mode terraform-mode go-autocomplete auto-complete go-mode company-tern xref-js2 rjsx-mode js2-refactor exec-path-from-shell ac-js2))))
;;(custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;; )

;; (cl-old-struct-compat-mode 1)

;; (add-hook 'before-save-hook 'gofmt-before-save)

;; (defun auto-complete-for-go ()
;;   (auto-complete-mode 1))
;; (add-hook 'go-mode-hook 'auto-complete-for-go)

;; (with-eval-after-load 'go-mode
;;    (require 'go-autocomplete))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (yasnippet-snippets autopair dockerfile-mode helm use-package lsp-mode company-tern xref-js2 js2-refactor multi-web-mode py-autopep8 flycheck elpy ein better-defaults yaml-mode go-mode rjsx-mode js-auto-beautify jupyter jsx-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; remember to run Run M-x jedi:install-server RET during installation.
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; yasnippet - remember to install the snippets yasnippet-snippets
(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-mode)
(require 'lsp-clients)
;; pip install python-language-server to get the server
(add-hook 'python-mode-hook #'lsp)
(add-hook 'java-mode-hook #'lsp)
(add-hook 'java-mode-hook 'flycheck-mode)
(add-hook 'java-mode-hook 'company-mode)
(require 'company-lsp)
(push 'company-lsp company-backends)

(require 'use-package)
(use-package hydra)

(use-package helm)

(use-package helm-lsp
  :config
  (defun netrom/helm-lsp-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-workspace-symbol)))

  (defun netrom/helm-lsp-global-workspace-symbol-at-point ()
    (interactive)
    (let ((current-prefix-arg t))
      (call-interactively #'helm-lsp-global-workspace-symbol))))

(require 'lsp-mode)
(require 'hydra)
(require 'helm)
(require 'helm-lsp)
(setq netrom--general-lsp-hydra-heads
      '(;; Xref
        ("d" xref-find-definitions "Definitions" :column "Xref")
        ("D" xref-find-definitions-other-window "-> other win")
        ("r" xref-find-references "References")
        ("s" netrom/helm-lsp-workspace-symbol-at-point "Helm search")
        ("S" netrom/helm-lsp-global-workspace-symbol-at-point "Helm global search")

        ;; Peek
        ("C-d" lsp-ui-peek-find-definitions "Definitions" :column "Peek")
        ("C-r" lsp-ui-peek-find-references "References")
        ("C-i" lsp-ui-peek-find-implementation "Implementation")

        ;; LSP
        ("p" lsp-describe-thing-at-point "Describe at point" :column "LSP")
        ("C-a" lsp-execute-code-action "Execute code action")
        ("R" lsp-rename "Rename")
        ("t" lsp-goto-type-definition "Type definition")
        ("i" lsp-goto-implementation "Implementation")
        ("f" helm-imenu "Filter funcs/classes (Helm)")
        ("s" lsp-ui-find-workspace-symbol "Find the symbol in the working space.")
        ("C-c" lsp-describe-session "Describe session")

        ;; Flycheck
        ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck")))

(setq netrom--misc-lsp-hydra-heads
      '(;; Misc
        ("q" nil "Cancel" :column "Misc")
        ("b" pop-tag-mark "Back")))

;; Create general hydra.
(eval `(defhydra netrom/lsp-hydra (:color blue :hint nil)
         ,@(append
            netrom--general-lsp-hydra-heads
            netrom--misc-lsp-hydra-heads)))

(add-hook 'lsp-mode-hook
          (lambda () (local-set-key (kbd "C-c C-l") 'netrom/lsp-hydra/body)))

(use-package company
  :config
  (setq company-idle-delay 0.3)

  (global-company-mode 1)

  (global-set-key (kbd "C-<tab>") 'company-complete))

(use-package company-lsp
  :requires company
  :config
  (push 'company-lsp company-backends)

   ;; Disable client-side cache because the LSP server does a better job.
  (setq company-transformers nil
        company-lsp-async t
        company-lsp-cache-candidates nil))

(use-package projectile
  :config
  (projectile-mode +1)
  (define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
)
(require 'helm-projectile)
(helm-projectile-on)


;;; https://scalameta.org/metals/docs/editors/emacs.html
;; NOTE: Delete ensime package in .emacs.d to enable metals for scala.
(use-package scala-mode
  :interpreter
    ("scala" . scala-mode))
(use-package lsp-mode
  ;; Optional - enable lsp-mode automatically in scala files
  :hook (scala-mode . lsp)
  :config (setq lsp-prefer-flymake nil))
(use-package lsp-ui)
(use-package company-lsp)

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map)
   ;; sbt-supershell kills sbt-mode:  https://github.com/hvesalai/emacs-sbt-mode/issues/152
   (setq sbt:program-options '("-Dsbt.supershell=false"))
)

;; configure the java mode
(add-hook 'java-mode-hook (lambda ()
                            (setq c-basic-offset 4
                                  tab-width 4
                                  indent-tabs-mode t)))

;; enable autopair in all buffers
(require 'autopair)
(autopair-global-mode)
(show-paren-mode 1)
