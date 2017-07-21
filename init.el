(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(defvar emacs|gc-cons-threshold (* 100 1024 1024))
;; Save `file-name-handler-alist' temporarily and set it to nil which
;; means on every .el and .elc file loaded during start up, it hasn't
;; to runs those regexps against the filename.
(defvar emacs|file-name-handler-alist file-name-handler-alist)
;; Local packages in this directory
(defvar emacs|site-packages-directory
  (expand-file-name "site-lisp" user-emacs-directory))
;; Some configuration file in this directory
(defvar emacs|etc-direcotry (expand-file-name "etc" user-emacs-directory))
;; All data and external executable file in this direcotry
(defvar emacs|var-direcotry (expand-file-name "var" user-emacs-directory))
;; All configuration in this directory
(defvar emacs|config-directory (expand-file-name "lisp" user-emacs-directory))

;; Font size and family seeting
(defvar emacs|default-font-name "Ubuntu Mono-13")

;; When buffer's size bigger than `emacs|large-buffer-size', it will
;; close some features to speed up emacs performance
(defvar emacs|large-buffer-size (* 1024 1024))

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold emacs|gc-cons-threshold)
;; Add `emacs|config-directory' to `load-path'
(add-to-list 'load-path emacs|config-directory)

;;* Core features {{
;; Some important tool (function)
(require 'init-utils)
;; Set some important variables
;; Load all packages
(require 'init-packages)
(ignore-errors (load-file (core|expand-var "init-env-vars.el")))
(require 'init-vars)
(require 'init-auto-mode)
(require 'init-defaults)
;; setup emacs outlooking
(require 'init-color-theme)
(require 'init-modeline)
;; }}

;;* Main features {{
(require 'init-ivy)
(require 'init-company)
(require 'init-hydra)
(require 'init-term-mode)
;; 'yasnippet', 'flycheck' ...
(require 'init-main-misc)
(require 'init-semantic)
(require 'init-linum-mode)
(require 'init-hs-minor-mode)
;; }}

(if git|has-git-p
    (require 'init-git))
(if (or spelling|has-aspell-p
        spelling|has-hunspell-p)
    (require 'init-spelling))
(require 'init-python-mode)
(require 'init-lisp)
(require 'init-web-mode)
(require 'init-sh)

;; Some keybindings using when editing
(require 'init-editing)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-windows)

(require 'init-org)
(require 'init-css)
(require 'init-haskell)
(require 'init-cc-mode)
(require 'init-javascript)
(require 'init-tags)
(require 'init-gud)
(require 'init-latex)

;; Other small tools
(require 'init-extra)

;; Locales (setting them earlier in this file doesn 't work in X)
(when (or window-system emacs|locale-is-utf8-p)
  (setq locale-coding-system 'utf-8)
  (set-default-coding-systems 'utf-8)
  (unless (eq system-type 'windows-nt)
    (set-selection-coding-system 'utf-8))
  (prefer-coding-system 'utf-8))

;; Load private configuration
(ignore-errors
  (load-file (expand-file-name "private.el" user-emacs-directory)))

;; restore `file-name-handler-alist'
(setq file-name-handler-alist emacs|file-name-handler-alist)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(session-use-package t nil (session)))

(put 'scroll-left 'disabled nil)
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
