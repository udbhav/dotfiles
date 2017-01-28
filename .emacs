;;; emacs-config --- for Udbhav Gupta

;;; Commentary:
;; lives in ~/, also symlink Emacs directory to your home

;;; Code:

;; start emacs-server!
(server-start)

(progn (cd "~/.emacs-packages") (normal-top-level-add-subdirs-to-load-path))
(add-to-list 'load-path "~/.emacs-packages")

;; PACKAGE MANAGEMENT

(require 'use-package)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

(when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
(package-initialize)

;; GLOBAL CONFIG

;; predictive completion
(use-package pabbrev
  :init (setq pabbrev-read-only-error nil)
  :ensure t)

;; ido (for completion in opening and switching buffers)
(require 'ido)
(ido-mode t)

;; Tabbing
(setq tab-width 2); ;; Tab width
(setq-default indent-tabs-mode nil); ;; Use spaces for tabs only!
(setq css-indent-offset 2)
(setq js-indent-level 2)

;; no backups
(setq make-backup-files nil)

;; don't autosave in project dirs
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))

;; Set a default starting directory
;; (setq default-directory "~/Sites/" )

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

;; delete trailing whitespace on save
;;(add-hook 'before-save-hook 'delete-trailing-whitespace)

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

;; Use Emacs terminfo, not system terminfo
(setq system-uses-terminfo nil)

;; window navigation
(defun other-window-backward ()
  "Goto previous window"
  (interactive)
  (other-window -1))
(global-set-key (kbd "\C-x p") 'other-window-backward)2

;; query-replace
(global-set-key (kbd "C-c r") 'query-replace)

;; spacey
;; (require 'zone)
;; (zone-when-idle 900)

;; PLATFORM CONFIG

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

;; LINTING

;; http://www.flycheck.org/manual/latest/index.html
(use-package flycheck
  :ensure
  :config
  (global-flycheck-mode)
  ;; disable jshint since we prefer eslint checking
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint)))

  ;; use eslint with web-mode for jsx files
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; customize flycheck temp file prefix
  (setq-default flycheck-temp-prefix ".flycheck")

  ;; disable json-jsonlist checking for json files
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(json-jsonlist)))

  ;; use local eslint executable
  (defun my/use-eslint-from-node-modules ()
    (let* ((root (locate-dominating-file
                  (or (buffer-file-name) default-directory)
                  "node_modules"))
           (eslint (and root
                        (expand-file-name "node_modules/eslint/bin/eslint.js"
                                          root))))
      (when (and eslint (file-executable-p eslint))
        (setq-local flycheck-javascript-eslint-executable eslint))))
  (add-hook 'flycheck-mode-hook #'my/use-eslint-from-node-modules)

  :init (add-hook 'after-init-hook #'global-flycheck-mode)
)



;; MODE CONFIGS

;; WEB

(use-package web-mode
  :ensure t)

(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.liquid?\\'" . web-mode))

;; indent 2 space in web mode
(defun web-mode-indent-hook ()
  "Hooks for Web mode."
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-css-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
)
(add-hook 'web-mode-hook 'web-mode-indent-hook)

;; don't show lines-tail for html
(defun web-mode-whitespace-hook ()
  (if (equal web-mode-content-type "html")
      (setq whitespace-style '(face empty trailing))))
(add-hook 'web-mode-hook 'web-mode-whitespace-hook)

;; jsx for all javascript
(defun web-mode-jsx-hook ()
  (if (equal web-mode-content-type "javascript")
      (web-mode-set-content-type "jsx")))
(add-hook 'web-mode-hook 'web-mode-jsx-hook)

;; CSS

(use-package css-mode
  :ensure t)

(use-package sass-mode
  :ensure t)

;; scss and less files are sass or css mode
(setq auto-mode-alist (cons '("\\.scss$" . sass-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.scss.erb$" . sass-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.css.erb$" . css-mode) auto-mode-alist))
(setq auto-mode-alist (cons '("\\.less$" . css-mode) auto-mode-alist))

;; MARKDOWN

(use-package markdown-mode
  :ensure t)

;; (autoload 'markdown-mode "markdown-mode"
;;    "Major mode for editing Markdown files" t)
(add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;; spell checking in markdown
(dolist (hook '(markdown-mode-hook))
  (add-hook hook (lambda () (flyspell-mode 1))))

;; RUBY

(add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))
(add-to-list 'auto-mode-alist '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode))

;; don't indent arguments all the way in for ruby
(setq ruby-deep-indent-paren nil)

;; PYTHON

;; python mode for wsgi
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

;; SWIFT

(use-package swift-mode
  :ensure t)
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

;; JSON
(use-package json-mode :ensure)

;; YAML
(use-package yaml-mode :ensure)

;; APPEARANCE

;; colors
(use-package color-theme
  :init (load-theme 'wombat t)
  :ensure t)

;; remove menu and startup screen
(menu-bar-mode -1)
(custom-set-variables
 '(inhibit-startup-screen t))

;; term colors
'(term-default-fg-color ((t (:inherit term-color-white))))
'(term-default-bg-color ((t (:inherit term-color-black))))
`(term-color-red ((t (:foreground ,"#e5786d"
                                  :background ,"#e5786d"))))

;; show line number and column number
(setq line-number-mode t)
(setq column-number-mode t)

;; highlight parentheses
(show-paren-mode 1)

;; scratch message
(setq initial-scratch-message nil)

;; turn sound off
(setq bell-volume 0)
(setq sound-alist nil)

(require 'whitespace)
(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)
