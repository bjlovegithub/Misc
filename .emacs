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

(global-eldoc-mode nil)
(add-to-list 'default-frame-alist '(fullscreen . maximized))

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
    treemacs
    dap-mode
    counsel-etags
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

;; create tags
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (eshell-command
   (format "find %s -type f -name \"*.*\" | etags -" dir-name)))

(defadvice find-tag (around refresh-etags activate)
  "Rerun etags and reload tags if tag not found and redo find-tag.
   If buffer is modified, ask about save before running etags."
  (let ((extension (file-name-extension (buffer-file-name))))
    (condition-case err
        ad-do-it
      (error (and (buffer-modified-p)
                  (not (ding))
                  (y-or-n-p "Buffer is modified, save it? ")
                  (save-buffer))
             (er-refresh-etags extension)
             ad-do-it))))
(defun er-refresh-etags (&optional extension)
  "Run etags on all peer files in current dir and reload them silently."
  (interactive)
  (shell-command (format "etags *.%s" (or extension "el")))
  (let ((tags-revert-without-query t))  ; don't query, revert silently
    (visit-tags-table default-directory nil)))


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
     (php-mode counsel-etags treemacs-magit treemacs-icons-dired treemacs-projectile treemacs-evil yasnippet-snippets autopair dockerfile-mode helm use-package lsp-mode company-tern xref-js2 js2-refactor multi-web-mode py-autopep8 flycheck elpy ein better-defaults yaml-mode go-mode rjsx-mode js-auto-beautify jupyter jsx-mode))))
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

;; (use-package helm)

;; (use-package helm-lsp
;;   :config
;;   (defun netrom/helm-lsp-workspace-symbol-at-point ()
;;     (interactive)
;;     (let ((current-prefix-arg t))
;;       (call-interactively #'helm-lsp-workspace-symbol)))

;;   (defun netrom/helm-lsp-global-workspace-symbol-at-point ()
;;     (interactive)
;;     (let ((current-prefix-arg t))
;;       (call-interactively #'helm-lsp-global-workspace-symbol))))

(require 'lsp-mode)
(require 'hydra)
;(require 'helm)
;(require 'helm-lsp)
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
        ("a" lsp-java-add-import)

        ;; Flycheck
        ("l" lsp-ui-flycheck-list "List errs/warns/notes" :column "Flycheck")))

(setq netrom--misc-lsp-hydra-heads
      '(;; Misc
        ("q" nil "Cancel" :column "Misc")
        ("b" pop-tag-mark "Back")))

'(lsp-enable-snippet nil)
'(lsp-ui-doc-delay 1)
'(lsp-ui-doc-max-height 30)
'(lsp-ui-sideline-delay 2)
'(lsp-ui-sideline-show-code-actions nil)
'(lsp-ui-sideline-show-hover nil)
'(lsp-ui-doc-background "#61AFEF")
(setq lsp-print-performance t)
(setq company-lsp-cache-candidates t)

(use-package lsp-ui
  :after (lsp)
  :init
  (setf lsp-ui-sideline-enable nil)
  (when (require 'xwidget nil 'noerror)
    (setf lsp-ui-doc-use-webkit t))
  (when lsp-ui-doc-use-webkit
    (setf lsp-ui-doc-enable t)
    (setf lsp-ui-doc-position 'at-point)
    (setf lsp-ui-doc-header nil)
    (setf lsp-ui-doc-include-signature t))
  :config
  (define-key lsp-ui-mode-map [remap xref-find-definitions] #'lsp-ui-peek-find-definitions)
  (define-key lsp-ui-mode-map [remap xref-find-references] #'lsp-ui-peek-find-references)
  (add-hook 'lsp-after-open-hook #'lsp-ui-mode))

(lsp-ui-doc--delete-frame)
(custom-set-faces '(nobreak-space ((t nil))))
'(lsp-ui-doc-use-childframe t)
'(lsp-ui-doc-use-webkit t)

(setq-default lsp-ui-doc-frame-parameters
                '((left . -1)
                  (top . -1)
                  (no-accept-focus . t)
                  (min-width . 0)
                  (width . 0)
                  (min-height . 0)
                  (height . 0)
                  (internal-border-width . 0)
                  (vertical-scroll-bars)
                  (horizontal-scroll-bars)
                  (left-fringe . 0)
                  (right-fringe . 0)
                  (menu-bar-lines . 0)
                  (tool-bar-lines . 0)
                  (line-spacing . 0.1)
                  (unsplittable . t)
                  (undecorated . t)
                  (minibuffer . nil)
                  (visibility . nil)
                  (mouse-wheel-frame . nil)
                  (no-other-frame . t)
                  (cursor-type)
                  (no-special-glyphs . t)))

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

;; conf for the treemacs
(use-package treemacs
  :ensure t
  :defer t
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if treemacs-python-executable 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-missing-project-action        'ask
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-position                      'left
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-asc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null treemacs-python-executable)))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple))))
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-evil
  :after treemacs evil
  :ensure t)

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

;; set up the dap mode
(require 'dap-java)
;;pip install "ptvsd>=4.2"
(require 'dap-python)


(defun fix-c-indent-offset-according-to-syntax-context (key val)
  ;; remove the old element
  (setq c-offsets-alist (delq (assoc key c-offsets-alist) c-offsets-alist))
  ;; new value
  (add-to-list 'c-offsets-alist '(key . val)))

;; conf the indentation
;; Java
;; use C-c C-o to set offset
;; use C-c C-s to show syntactic information (show the variable that needs to be set)
(add-hook 'java-mode-hook (lambda ()
                            (setq c-default-style "java")
                            (c-set-offset 'arglist-intro '+)
                            (c-set-offset 'arglist-close '0)
                            (c-set-offset 'case-label '+)
                            (auto-complete-mode t)
                            ))
