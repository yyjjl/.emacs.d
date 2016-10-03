;; -*- coding: utf-8 -*-

 ;; don't GC during startup to save time
(setq gc-cons-threshold (* 100 1024 1024))

(defvar file-name-handler-alist-tmp file-name-handler-alist
  "Save file-name-handler-alist temporarily and set it to nil which means on every .el and .elc file loaded during start up, it hasn't to runs those regexps against the filename.")
(setq file-name-handler-alist nil)

(add-to-list 'load-path (expand-file-name "~/.emacs.d/lisp"))
;;----------------------------------------------------------------------------
;; Which functionality to enable (use t or nil for true and false)
;;----------------------------------------------------------------------------
(setq *is-a-mac* (eq system-type 'darwin))
(setq *win64* (eq system-type 'windows-nt) )
(setq *cygwin* (eq system-type 'cygwin) )
(setq *linux* (or (eq system-type 'gnu/linux) (eq system-type 'linux)) )
(setq *unix* (or *linux* (eq system-type 'usg-unix-v) (eq system-type 'berkeley-unix)) )
(setq *emacs24* (and (not (featurep 'xemacs)) (or (>= emacs-major-version 24))) )
(setq *no-memory* (cond
                   (*is-a-mac*
                    (< (string-to-number (nth 1 (split-string (shell-command-to-string "sysctl hw.physmem")))) 4000000000))
                   (*linux* nil)
                   (t nil)))

(eval-when-compile
  (require 'cl)
  (require 'cl-lib))

(load-file "~/.emacs.d/packages.el")

(if (fboundp 'normal-top-level-add-to-load-path)
    (let* ((my-lisp-dir "~/.emacs.d/site-lisp/")
           (default-directory my-lisp-dir))
      (progn
        (setq load-path
              (append
               (loop for dir in (directory-files my-lisp-dir)
                     unless (string-match "^\\." dir)
                     collecting (expand-file-name dir))
               load-path)))))

;; setup packages
(require 'init-elpa)

(eval-when-compile
  (require 'bind-key))

(require 'benchmark-init)

;; some important tool function
;; I need immediately
(require 'init-utils)
(require 'init-defaults)

;; setup emacs outlooking
(require 'init-color-theme)
(require 'init-gui-frames)
(require 'init-modeline)

;; Set up $PATH
(require 'init-exec-path)
;; any file use flyspell should be initialized after init-spelling.el
;; actually, I don't know which major-mode use
(require 'init-spelling)
(require 'init-dired)
(require 'init-ibuffer)
(require 'init-flycheck)
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
(require 'init-gud)
(require 'init-linum-mode)
(require 'init-sh)
(require 'init-tags)
(require 'init-term-mode)
(require 'init-web-mode)
(require 'init-slime)
(require 'init-glsl-mode)
(require 'init-company)
(require 'init-tide)
(require 'init-latex)
(require 'init-semantic)
;; need statistics of keyfreq asap
(require 'init-keyfreq)

;; misc has some crucial tools I need immediately
(require 'init-clipboard)
(require 'init-workgroups2)
(require 'init-windows)
(require 'init-hs-minor-mode)
(require 'init-misc)

(require 'after-init)
(require 'idle-require)
(setq idle-require-idle-delay 1)
(setq idle-require-symbols '(init-misc-lazy
                             init-doxygen))
(idle-require-mode 1)

;;----------------------------------------------------------------------------
;; Locales (setting them earlier in this file doesn't work in X)
;;----------------------------------------------------------------------------
(require 'init-locales)

;; my personal setup, other major-mode specific setup need it.
;; It's dependent on init-site-lisp.el
;; (if (file-exists-p "~/.emacs.d/custom.el")
;;     (load-file "~/.emacs.d/custom.el"))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(abbrev-file-name "~/.emacs.d/data/abbrev_defs")
 '(bmkp-bmenu-state-file "~/.emacs.d/data/bmk-bmenu-state.el")
 '(bmkp-last-as-first-bookmark-file "/home/yyj/.emacs.d/data/bookmarks")
 '(clang-format-executable "clang-format-3.7")
 '(clang-format-style "{BasedOnStyle: Google,IndentWidth: 4}")
 '(company-statistics-file "~/.emacs.d/data/company-statistics-cache.el")
 '(elpy-rpc-python-command "python3")
 '(git-gutter:handled-backends (quote (svn hg git)))
 '(ispell-personal-dictionary "~/.emacs.d/data/aspell.pws")
 '(keyfreq-file "~/.emacs.d/data/keyfreq")
 '(python-shell-interpreter "python3")
 '(recentf-save-file "~/.emacs.d/data/recentf")
 '(safe-local-variable-values (quote ((lentic-init . lentic-orgel-org-init))))
 '(savehist-file "~/.emacs.d/data/history")
 '(send-mail-function (quote smtpmail-send-it))
 '(session-save-file "~/.emacs.d/data/session")
 '(session-use-package t nil (session))
 '(smex-save-file "~/.emacs.d/data/smex-items")
 '(srecode-map-save-file "~/.emacs.d/data/srecode-map.el"))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-scrollbar-bg ((t (:background "#3f4346"))))
 '(company-scrollbar-fg ((t (:background "#333638"))))
 '(company-tooltip ((t (:inherit default :background "#27292b"))))
 '(company-tooltip-common ((t (:inherit font-lock-constant-face))))
 '(company-tooltip-selection ((t (:inherit font-lock-keyword-face :background "#4b5053")))))

(setq file-name-handler-alist file-name-handler-alist-tmp)

;;; Local Variables:
;;; no-byte-compile: t
;;; End:
(put 'scroll-left 'disabled nil)
;; Don't disable narrowing commands
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;; Ctrl-X, u/l  to upper/lowercase regions without confirm
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'erase-buffer 'disabled nil)
