;; start emacs-server!
(server-start)

(add-to-list 'load-path "~/emacs")

(progn (cd "~/emacs") (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/emacs")

;; colors
(load-theme 'wombat t)

;; term colors
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))

`(term-color-red ((t (:foreground ,"#e5786d"
                                  :background ,"#e5786d"))))

;; php mode
(load "php-mode")
(add-to-list 'auto-mode-alist
	     '("\\.php[34]?\\'\\|\\.phtml\\'" . php-mode))

(custom-set-variables
 '(inhibit-startup-screen t))

;; remove menu
(menu-bar-mode -1)

;; ido (for completion in opening and switching buffers)
(require 'ido)
(ido-mode t)

;; Tabbing
(setq default-tab-width 2); ;; Tab width
(setq-default indent-tabs-mode nil); ;; Use spaces for tabs only!
(setq js-indent-level 2)
(setq css-indent-offset 2)

(setq line-number-mode t)
(setq column-number-mode t)

;; predictive completion
(require 'pabbrev )
(setq pabbrev-read-only-error nil)

;; javascript mode
(autoload 'javascript-mode "javascript" nil t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . javascript-mode))

;; django-mode
(load "django-mode.elc")

;; haml-mode
(require 'haml-mode)

;; coffeescript
(require 'coffee-mode)
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))

;; bind comment and uncomment to C-;
(global-set-key (kbd "C-c C-c") 'comment-or-uncomment-region)
;; (global-set-key (kbd "C-;") 'comment-or-uncomment-region)

;; don't autosave in project dirs
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; no backups either
(setq make-backup-files nil)

;; Set a default starting directory
(setq default-directory "~/Sites/" )

;; highlight parentheses
(show-paren-mode 1)

;; Don't make me type yes or no in full!
(defalias 'yes-or-no-p 'y-or-n-p)

;; rectangle support without the silly keys
(setq cua-enable-cua-keys nil)

;; UTF-8!
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; No more double slashes in re-builder
(setq reb-re-syntax 'string)

;; Hippie Expand
(global-set-key (kbd "M-/") 'hippie-expand)
(setq hippie-expand-try-functions-list '(try-expand-dabbrev try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill try-complete-file-name-partially try-complete-file-name try-expand-all-abbrevs try-expand-list try-expand-line try-complete-lisp-symbol-partially try-complete-lisp-symbol))

;; Org-mode settings
(setq load-path (cons "~/emacs/org" load-path))
(require 'org-install)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
;;(setq org-log-done t)
;;(setq org-startup-indented t)
;;(setq org-hide-leading-stars t)
(add-hook 'org-mode-hook 'turn-on-font-lock)

;; scratch message
(setq initial-scratch-message nil)

;; turn sound off
(setq bell-volume 0)
(setq sound-alist nil)

(require 'sass-mode)

(require 'yaml-mode)
    (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))

;; liquid files are html mode
(setq auto-mode-alist (cons '("\\.liquid$" . html-mode) auto-mode-alist))

;; scss and less files are css mode
(setq auto-mode-alist (cons '("\\.scss$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.scss.erb$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css.erb$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.less$" . css-mode) auto-mode-alist))

;; delete trailing whitespace on save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; 80 column rule
(require 'whitespace)
(setq whitespace-style '(face empty trailing))
(global-whitespace-mode t)

;; ruby mode for all those files
(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode))

;; don't indent arguments all the way in for ruby
(setq ruby-deep-indent-paren nil)

;; python mode for wsgi
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

;; copy and paste to and from os x
(defun copy-from-osx ()
  (shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(setq interprogram-cut-function 'paste-to-osx)
(setq interprogram-paste-function 'copy-from-osx)


;; terminal in emacs
(defun visit-term-buffer ()
  "Create or visit a terminal buffer."
  (interactive)
  (if (not (get-buffer "*ansi-term*"))
      (progn
        (split-window-sensibly (selected-window))
        (other-window 1)
        (ansi-term (getenv "SHELL")))
    (switch-to-buffer-other-window "*ansi-term*")))

(global-set-key (kbd "C-c t") 'visit-term-buffer)

;; window navigation
(defun other-window-backward ()
  "Goto previous window"
  (interactive)
  (other-window -1))
(global-set-key (kbd "\C-x p") 'other-window-backward)2

;; spacey
(require 'zone)
(zone-when-idle 900)

;; query-replace
(global-set-key (kbd "C-c r") 'query-replace)

;; markdown
(autoload 'markdown-mode "markdown-mode"
   "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
