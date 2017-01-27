;; -*- coding: utf-8 -*-

 ;; don't GC during startup to save time
(setq gc-cons-threshold (* 100 1024 1024))
;;Save file-name-handler-alist temporarily and set it to nil which means on every
;; .el and .elc file loaded during start up, it hasn't to runs those regexps
;; against the filename.
(defvar file-name-handler-alist-tmp file-name-handler-alist)
(defvar package-use-priority nil  "whether to use priority")
(defvar site-packages-directory "~/.emacs.d/site-lisp/")

(setq file-name-handler-alist nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

;; all packages
(load-file "~/.emacs.d/packages.el")
(if (fboundp 'normal-top-level-add-to-load-path)
    (let ((default-directory site-packages-directory))
      (setq load-path
            (append load-path
                    (loop for dir in (directory-files
                                      (expand-file-name site-packages-directory))
                          unless (string-match "^\\." dir)
                          collecting (expand-file-name dir))))))

;; need bind-key immediately
(require 'bind-key)

;; some important tool function
(require 'init-utils)
(require 'init-defaults)
;; setup emacs outlooking
(require 'init-color-theme)
(require 'init-gui-frames)
(require 'init-modeline)
;; Set up $PATH
(require 'init-exec-path)
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
(require 'init-term-mode)
(require 'init-web-mode)
(require 'init-slime)
(require 'init-gud)
(require 'init-tide)
(require 'init-latex)
(require 'init-semantic)
;; need statistics of keyfreq
;; (require 'init-keyfreq)

;; misc has some crucial tools I need immediately
(require 'init-clipboard)
(require 'init-windows)
(require 'init-hs-minor-mode)
(require 'init-misc)
(require 'init-doxygen)
(require 'after-init)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;; load org project file
(if (file-exists-p "~/.emacs.d/org-project.el")
    (load-file "~/.emacs.d/org-project.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/data/abbrev_defs")
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-save-list/.saves-")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file "/home/yyj/.emacs.d/data/bookmarks")
 '(company-statistics-file "~/.emacs.d/data/company-statistics-cache.el")
 '(ispell-personal-dictionary "~/.emacs.d/data/aspell.pws")
 '(keyfreq-file "~/.emacs.d/data/keyfreq")
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(savehist-file "~/.emacs.d/data/history")
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb/")
 '(session-save-file "~/.emacs.d/data/session")
 '(session-use-package t nil (session))
 '(smex-save-file "~/.emacs.d/data/smex-items")
 '(srecode-map-save-file "~/.emacs.d/data/srecode-map.el")
 '(tramp-persistentcy-file-name "~/.emacs.d/data/tramp"))

(setq file-name-handler-alist file-name-handler-alist-tmp)
;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'scroll-left 'disabled nil)
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
