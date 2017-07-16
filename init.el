;; -*- coding: utf-8 -*-

;; don't GC during startup to save time
(setq gc-cons-threshold (* 100 1024 1024))
;; save file-name-handler-alist temporarily and set it to nil
;; which means on every .el and .elc file loaded during start up,
;; it hasn't to runs those regexps against the filename.
(defvar file-name-handler-alist-tmp file-name-handler-alist)
(defvar emacs-site-packages-directory
  (expand-file-name "site-lisp" user-emacs-directory))
(defvar emacs-etc-direcotry (expand-file-name "etc" user-emacs-directory))
(defvar emacs-var-direcotry (expand-file-name "var" user-emacs-directory))
(defvar emacs-config-directory (expand-file-name "lisp" user-emacs-directory))

(defvar symbol-font-name "Noto Sans S Chinese")
(defvar symbol-font-size 18)
(defvar default-font-name "Ubuntu Mono-13")
(defvar large-buffer-size (* 1024 1024))
(defvar use-fcitx-setup-p t)

(setq file-name-handler-alist nil)

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(add-to-list 'load-path emacs-config-directory)
;; all packages
(if (fboundp 'normal-top-level-add-to-load-path)
    (let ((default-directory emacs-site-packages-directory))
      (setq load-path
            (append load-path
                    (loop for dir in
                          (directory-files
                           (expand-file-name emacs-site-packages-directory))
                          unless (string-match "^\\." dir)
                          collecting (expand-file-name dir))))))

(require 'init-packages)
;; some important tool function
(require 'init-vars)
(require 'init-utils)
(require 'init-defaults)
(require 'init-auto-mode)
;; setup emacs outlooking
(require 'init-color-theme)
(require 'init-modeline)
;; Set up $PATH
;; (require 'init-exec-path)
;; any file use flyspell should be initialized after init-spelling.el
(require 'init-spelling)
(require 'init-company)
(require 'init-flycheck)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-ivy)
(require 'init-git)
(require 'init-editing)
(require 'init-hippie-expand)
(require 'init-hydra)
(require 'init-markdown)
(require 'init-org)
(require 'init-css)
(require 'init-python-mode)
(require 'init-haskell)
(require 'init-yasnippet)
(require 'init-zencoding-mode)
(require 'init-cc-mode)
(require 'init-lisp)
(require 'init-javascript)
(require 'init-linum-mode)
(require 'init-sh)
(require 'init-tags)
;; (require 'init-eshell)
(require 'init-term-mode)
(require 'init-web-mode)
;; (require 'init-slime)
(require 'init-gud)
(require 'init-latex)
(require 'init-semantic)
(require 'init-clipboard)
(require 'init-windows)
(require 'init-hs-minor-mode)
(require 'init-doxygen)
;; (require 'init-ein)
(require 'init-extra)
(require 'after-init)

;;--------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;--------------------------------------------------------
(require 'init-locales)

;; load org project file
(if (file-exists-p "~/.emacs.d/private.el")
    (load-file "~/.emacs.d/private.el"))


(setq file-name-handler-alist file-name-handler-alist-tmp)
(put 'scroll-left 'disabled nil)
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)