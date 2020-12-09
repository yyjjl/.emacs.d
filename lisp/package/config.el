;; -*- lexical-binding: t; -*-

(setq package-quickstart t)

;; The index of archive represents its priority
(setq package-archives
      '(("melpa-stable" . "https://elpa.emacs-china.org/melpa-stable/")
        ("gnu" . "https://elpa.emacs-china.org/gnu/")
        ("melpa" . "https://elpa.emacs-china.org/melpa/")
        ("org" . "https://elpa.emacs-china.org/org/")))
;; Setup to select right archive
(setq ymacs-package--priority-alist (mapcar #'car package-archives))

(unless ymacs-use-gnutls-p
  (dolist (item package-archives)
    (setcdr item (replace-regexp-in-string "https:" "http:" (cdr item)))))

(if ymacs-dump-p
    (setq load-path ymacs-dump-load-path)
  ;; add load-path’s and load autoload files
  (package-initialize))

(require-packages! dash)
(require 'dash)
