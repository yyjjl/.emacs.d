;; -*- coding: utf-8 -*-

;; don't GC during startup to save time
(setq gc-cons-threshold (* 100 1024 1024))
;; save file-name-handler-alist temporarily and set it to nil
;; which means on every .el and .elc file loaded during start up,
;; it hasn't to runs those regexps against the filename.
(defvar file-name-handler-alist-tmp file-name-handler-alist)
(defvar package-use-priority nil  "whether to use priority")
(defvar site-packages-directory "~/.emacs.d/site-lisp/")
(defvar symbol-font-name "Noto Sans S Chinese")
(defvar yasnippet-extra-dir "~/.emacs.d/snippets")
(defvar large-buffer-size (* 1024 1024))

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-13"))

(setq file-name-handler-alist nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

;; all packages
(load-file "~/.emacs.d/lisp/packages.el")
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
;; (require 'init-tags)
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
;; (require 'init-misc)
(require 'init-extra)
(require 'after-init)

;;--------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;--------------------------------------------------------
(require 'init-locales)

;; load org project file
(if (file-exists-p "~/.emacs.d/private.el")
    (load-file "~/.emacs.d/private.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/data/abbrev_defs")
 '(anaconda-mode-installation-directory "~/.emacs.d/bin/anaconda-mode")
 '(auto-save-list-file-prefix "~/.emacs.d/data/auto-save-list/.saves-")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file "~/.emacs.d/data/bookmarks")
 '(cmake-ide-rdm-executable "~/.emacs.d/bin/rtags-build/bin/rdm")
 '(company-statistics-file "~/.emacs.d/data/company-statistics-cache.el")
 '(eshell-directory-name "~/.emacs.d/data/eshell/")
 '(flycheck-html-tidy-executable "~/.emacs.d/bin/tidy")
 '(geiser-repl-history-filename "~/.emacs.d/data/.geiser_history")
 '(hindent-process-path "~/.cabal/bin/hindent")
 '(irony-server-install-prefix "~/.emacs.d/bin/irony")
 '(irony-user-dir "~/.emacs.d/bin/irony/")
 '(ispell-personal-dictionary "~/.emacs.d/data/aspell.pws")
 '(keyfreq-file "~/.emacs.d/data/keyfreq")
 '(mc/list-file "~/.emacs.d/data/mc-lists.el")
 '(org-publish-timestamp-directory "~/.emacs.d/data/.org-timestamps/")
 '(projectile-known-projects-file "~/.emacs.d/data/projectile-bookmarks.eld")
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(rtags-path "~/.emacs.d/bin/rtags-build/bin")
 '(savehist-file "~/.emacs.d/data/history")
 '(semanticdb-default-save-directory "~/.emacs.d/data/semanticdb/")
 '(session-save-file "~/.emacs.d/data/session")
 '(session-use-package t nil (session))
 '(shm-program-name "~/.cabal/bin/structured-haskell-mode")
 '(smex-save-file "~/.emacs.d/data/smex-items")
 '(srecode-map-save-file "~/.emacs.d/data/srecode-map.el")
 '(tramp-persistentcy-file-name "~/.emacs.d/data/tramp"))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(setq file-name-handler-alist file-name-handler-alist-tmp)
(put 'scroll-left 'disabled nil)
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)
(put 'erase-buffer 'disabled nil)
(put 'LaTeX-narrow-to-environment 'disabled nil)

(setq enable-local-variables :all)
