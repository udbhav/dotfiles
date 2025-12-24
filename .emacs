;;; emacs-config --- for Udbhav Gupta

;;; Commentary:
;; lives in ~/, also symlink Emacs directory to your home

;;; Code:

;; start emacs-server!
 (server-start)

;; (progn (cd "~/.emacs-packages") (normal-top-level-add-subdirs-to-load-path))
;; (add-to-list 'load-path "~/.emacs-packages")

;; PACKAGE MANAGEMENT

(require 'use-package)
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/"))

;; (when (< emacs-major-version 24)
  ;; For important compatibility libraries like cl-lib
;;  (add-to-list 'package-archives '("gnu" . "https://elpa.gnu.org/packages/")))
;; (package-initialize)

;; GLOBAL CONFIG

;; predictive completion
;; (use-package pabbrev
;;  :init (setq pabbrev-read-only-error nil)
;; :ensure t)

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
(setq default-directory "~/Sites/" )

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
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; vterm
(use-package vterm
   :ensure t)
(use-package multi-vterm :ensure t)

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

(global-set-key (kbd "C-c t") 'multi-vterm)

;; Use Emacs terminfo, not system terminfo
;;(setq system-uses-terminfo nil)

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

(when (eq system-type 'darwin)
  (setq interprogram-cut-function 'paste-to-osx)
  (setq interprogram-paste-function 'copy-from-osx))

;; LINTING

;; http://www.flycheck.org/manual/latest/index.html
(use-package flycheck
 :ensure
 :config
 (global-flycheck-mode)
;;  disable jshint since we prefer eslint checking
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

 ;; use flake8 executable directly
 (setq flycheck-python-flake8-executable "flake8")

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

;; disable flymake
(flymake-mode -1)

;; MODE CONFIGS

;; WEB

;; (use-package web-mode
;;  :ensure t)

;; (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
;; ;; (add-to-list 'auto-mode-alist '("\\.jsx?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.liquid?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.liquid?\\'" . web-mode))
;;
;; indent 2 space in web mode

;; (defun web-mode-indent-hook ()
;;   "Hooks for Web mode."
;;   (setq web-mode-markup-indent-offset 2)
;;   (setq web-mode-css-indent-offset 2)
;;   (setq web-mode-code-indent-offset 2)
;; )
;; (add-hook 'web-mode-hook 'web-mode-indent-hook)
;;
;; ;; don't show lines-tail for html
;; (defun web-mode-whitespace-hook ()
;;   (if (equal web-mode-content-type "html")
;;       (setq whitespace-style '(face empty trailing))))
;; (add-hook 'web-mode-hook 'web-mode-whitespace-hook)
;;
;; ;; jsx for all javascript
;; ;; (defun web-mode-jsx-hook ()
;; ;;   (if (equal web-mode-content-type "javascript")
;; ;;       (web-mode-set-content-type "jsx")))
;; ;; (add-hook 'web-mode-hook 'web-mode-jsx-hook)
;;
;; ;; CSS
;;
;; (use-package css-mode
;;   :ensure t)
;;
;; (use-package sass-mode
;;   :ensure t)
;;
;; ;; scss and less files are sass or css mode
;; (setq auto-mode-alist (cons '("\\.scss$" . sass-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.scss.erb$" . sass-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.css.erb$" . css-mode) auto-mode-alist))
;; (setq auto-mode-alist (cons '("\\.less$" . css-mode) auto-mode-alist))
;;
;; ;; MARKDOWN
;;
;; (use-package markdown-mode
;;   :ensure t)
;;
;; ;; (autoload 'markdown-mode "markdown-mode"
;; ;;    "Major mode for editing Markdown files" t)
;; (add-to-list 'auto-mode-alist '("\\.text\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
;; (add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))
;;
;; ;; spell checking in markdown
;; (dolist (hook '(markdown-mode-hook))
;;   (add-hook hook (lambda () (flyspell-mode 1))))
;;
;; ;; RUBY
;;
;; (add-to-list 'auto-mode-alist '("\\.\\(rb\\|ru\\|builder\\|rake\\|thor\\|gemspec\\)\\'" . ruby-mode))
;; (add-to-list 'auto-mode-alist '("\\(rake\\|thor\\|guard\\|gem\\|cap\\|vagrant\\)file\\'" . ruby-mode))
;;
;; ;; don't indent arguments all the way in for ruby
;; (setq ruby-deep-indent-paren nil)
;;

;; Tree sitter setup
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-ts-mode))
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . tsx-ts-mode))
;; (add-to-list 'major-mode-remap-alist '(python-mode . python-ts-mode))

;; LSP
(use-package lsp-ui)

;; PYTHON

;; python mode for wsgi
(add-to-list 'auto-mode-alist '("\\.wsgi\\'" . python-mode))

;; enable blacken mode for python
(add-hook 'python-mode-hook
          (lambda ()
            (blacken-mode 1)
            ))
;;
;; ;; SWIFT
;;
;; (use-package swift-mode
;;   :ensure t)
;; (add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))
;;
;; ;; JSON
;; (use-package json-mode :ensure)
;;
;; ;; YAML
;; (use-package yaml-mode :ensure)
;;

;; APPEARANCE

;; colors
(add-to-list 'custom-theme-load-path (expand-file-name "~/.emacs.d/themes/"))
(load-theme 'nord t)

;; remove menu and startup screen
(menu-bar-mode -1)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(inhibit-startup-screen t)
 '(package-selected-packages
   '(blacken color-theme dockerfile-mode elpy flycheck js2-mode json-mode
             lsp-mode lsp-ui lua-mode magit markdown-mode multi-vterm
             nord-theme pabbrev php+-mode php-mode prettier-js
             protobuf-mode rjsx-mode sass-mode sqlformat swift-mode
             vterm web-mode xref-js2 yaml-mode)))
;;
;; ;; term colors
;; '(term-default-fg-color ((t (:inherit term-color-white))))
;; '(term-default-bg-color ((t (:inherit term-color-black))))
;; `(term-color-red ((t (:foreground ,"#e5786d"
;;                                   :background ,"#e5786d"))))
;;
;; ;; show line number and column number
;; (setq line-number-mode t)
;; (setq column-number-mode t)
;;
;; ;; highlight parentheses
;; (show-paren-mode 1)
;;
;; ;; scratch message
;; (setq initial-scratch-message nil)
;;
;; ;; turn sound off
;; (setq bell-volume 0)
;; (setq sound-alist nil)
;;
;; ;; magit config
;; (global-set-key (kbd "C-x g") 'magit-status)
;;
;; prettier mode
(require 'prettier-js)
(add-hook 'js-mode-hook 'prettier-js-mode)
;; ;; (add-hook 'web-mode-hook 'prettier-js-mode)
;;
;; (require 'whitespace)
;; (setq whitespace-style '(face empty lines-tail trailing))
;; (global-whitespace-mode t)
;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  )
;;
;;
;; ;; go to definition
;; (add-hook 'js-mode-hook 'js2-minor-mode)
;; (with-eval-after-load 'js
;;   (define-key js-mode-map (kbd "M-.") nil)
;; )
;; (add-hook 'js2-minor-mode-hook (lambda ()
;;   (add-hook 'xref-backend-functions #'xref-js2-xref-backend nil t)))
;;
;;
(use-package elpy
  :ensure t
  :config
  (remove-hook 'elpy-modules 'elpy-module-flymake)
  :init
  (elpy-enable))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
(put 'downcase-region 'disabled nil)
