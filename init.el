;; -*- coding: utf-8 -*-

;; don't GC during startup to save time
(setq gc-cons-threshold (* 100 1024 1024))
;; save file-name-handler-alist temporarily and set it to nil
;; which means on every .el and .elc file loaded during start up,
;; it hasn't to runs those regexps against the filename.
(defvar file-name-handler-alist-tmp file-name-handler-alist)
(defvar emacs-site-packages-directory (expand-file-name "~/.emacs.d/site-lisp/"))
(defvar emacs-yasnippet-extra-dir (expand-file-name "~/.emacs.d/snippets"))
(defvar emacs-data-directory (expand-file-name "~/.emacs.d/data"))

(defvar symbol-font-name "Noto Sans S Chinese")
(defvar large-buffer-size (* 1024 1024))
(defvar use-fcitx-setup-p t)

(add-to-list 'default-frame-alist '(font . "Ubuntu Mono-13"))

(setq file-name-handler-alist nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

;; all packages
(load-file "~/.emacs.d/lisp/packages.el")
(if (fboundp 'normal-top-level-add-to-load-path)
    (let ((default-directory emacs-site-packages-directory))
      (setq load-path
            (append load-path
                    (loop for dir in
                          (directory-files
                           (expand-file-name emacs-site-packages-directory))
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
 '(abbrev-file-name (in-data-directory "abbrev_defs"))
 '(auto-save-list-file-prefix (in-data-directory "auto-save-list/.saves-"))
 '(backup-directory-alist (list (cons "." (in-data-directory "backups"))))
 '(bookmark-default-file (in-data-directory "bookmarks"))
 '(cmake-ide-rdm-executable (in-data-directory "rtags-build/bin/rdm"))
 '(company-statistics-file (in-data-directory "company-statistics-cache.el"))
 '(eshell-directory-name (in-data-directory "eshell/"))
 '(flycheck-html-tidy-executable (in-data-directory "tidy"))
 '(geiser-repl-history-filename (in-data-directory ".geiser_history"))
 '(irony-server-install-prefix (in-data-directory "irony"))
 '(irony-user-dir (in-data-directory "irony/"))
 '(ispell-personal-dictionary (in-data-directory "aspell.pws"))
 '(keyfreq-file (in-data-directory "keyfreq"))
 '(mc/list-file (in-data-directory "mc-lists.el"))
 '(org-publish-timestamp-directory (in-data-directory ".org-timestamps/"))
 '(projectile-known-projects-file (in-data-directory "projectile-bookmarks.eld"))
 '(recentf-save-file (in-data-directory "recentf"))
 '(rtags-path (in-data-directory "rtags-build/bin"))
 '(save-place-file (in-data-directory "places"))
 '(savehist-file (in-data-directory "history"))
 '(semanticdb-default-save-directory (in-data-directory "semanticdb/"))
 '(session-save-file (in-data-directory "session"))
 '(session-use-package t nil (session))
 '(smex-save-file (in-data-directory "smex-items"))
 '(srecode-map-save-file (in-data-directory "srecode-map.el"))
 '(tramp-persistency-file-name (in-data-directory "tramp")))

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