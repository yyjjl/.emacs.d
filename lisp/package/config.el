;; -*- lexical-binding: t; -*-

(after! package
  ;; The index of archive represents its priority
  (setq package-archives
        '(("melpa-stable" . "http://elpa.emacs-china.org/melpa-stable/")
          ("gnu" . "http://elpa.emacs-china.org/gnu/")
          ("melpa" . "http://elpa.emacs-china.org/melpa/")
          ("org" . "http://elpa.emacs-china.org/org/")))
  ;; Setup to select right archive
  (setq ymacs-package--priority-alist (mapcar #'car package-archives))

  (unless ymacs-use-gnutls-p
    (dolist (item package-archives)
      (setcdr item (concat "http://"
                           (string-trim-left (cdr item) "https://"))))))

(if ymacs-dump-p
    (setq load-path ymacs-dump-load-path)
  ;; add load-path’s and load autoload files
  (package-initialize)

  (require 'dash)
  (require 'dash-functional))

;; Core packages
(require-packages!
 yasnippet
 yasnippet-snippets
 flycheck
 company
 session
 hydra
 pretty-hydra
 ivy
 ivy-hydra
 counsel
 swiper
 projectile
 counsel-projectile
 ;; `counsel-M-x' need amx to get history
 amx
 ;; Show key bindings when pressing
 which-key
 ;; Highlight braces with their depth
 rainbow-delimiters
 ;; Highlight indentation
 highlight-indentation
 ;; ^L beautifier
 page-break-lines
 evil-nerd-commenter
 shackle
 hl-todo
 hl-fill-column
 expand-region)
