(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(defvar emacs|gc-cons-threshold (* 1000 1024 1024))
;; Save `file-name-handler-alist' temporarily and set it to nil which
;; means on every .el and .elc file loaded during start up, it hasn't
;; to runs those regexps against the filename.
(defvar emacs|file-name-handler-alist file-name-handler-alist)
;; Local packages in this directory
(defvar emacs|site-packages-directory
  (expand-file-name "site-lisp" user-emacs-directory))
;; Some configuration file in this directory
(defvar emacs|etc-direcotry
  (expand-file-name "etc" user-emacs-directory))
;; All data and external executable file in this direcotry
(defvar emacs|var-direcotry (expand-file-name "var" user-emacs-directory))
;; All configuration in this directory
(defvar emacs|config-directory (expand-file-name "lisp" user-emacs-directory))
;; Font size and family seeting
(defvar emacs|default-font-name "Ubuntu Mono-13")

(setq file-name-handler-alist nil)
;; Don't GC during startup to save time
(setq gc-cons-threshold emacs|gc-cons-threshold)
;; Add `emacs|config-directory' to `load-path'
(add-to-list 'load-path emacs|config-directory)

;;* Core features {{
;; All packages required in this section are defined in `init-packages'
(require 'init-outlooking)
;; Some important tool functions
(require 'init-utils)

;; Set some important variables
;; Load all packages
;; (package-initialize)
(require 'init-packages)
(require 'init-vars)
(load-file (core|expand-var "init-env-vars.el"))
(require 'init-auto-mode)
(require 'init-defaults)
;; Setup emacs outlooking
(require 'init-color-theme)
(require 'init-modeline)
(require 'init-ivy)
(require 'init-company)
(require 'init-hydra)
(require 'init-term-mode)
;; `yasnippet', `flycheck' ...
(require 'init-main-misc)
(require 'init-semantic)
;; (require 'init-linum-mode)
(require 'init-hs-minor-mode)
;; }}

;;* Optional features {{
;; All packages required in this section are defined in their init files
(require 'init-org)

(when term|use-eshell-p
  (require 'init-eshell))

(when latex|use-latex-p
  (require 'init-latex))

(if git|has-git-p
    (require 'init-git))

(if (or spelling|has-aspell-p
        spelling|has-hunspell-p)
    (require 'init-spelling))

(if tags|has-ggtags-p
    (require 'init-tags))

;; Programming modes
(require 'init-cpp)
(require 'init-haskell)
(require 'init-python)
(require 'init-lisp)
(require 'init-web-mode)
(require 'init-sh)
(require 'init-javascript)
(require 'init-css)

;; Some keybindings using when editing
(require 'init-editing)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-windows)
(require 'init-gud)

;; Other small tools
(require 'init-extra)
;; }}

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
(ignore-errors (load-file custom-file))

;; restore `file-name-handler-alist'
(setq file-name-handler-alist emacs|file-name-handler-alist)

;; Don't disable narrowing commands
(put 'scroll-left 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'TeX-narrow-to-group 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)
