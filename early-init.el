;; -*- lexical-binding: t -*-

(defvar ymacs-dump-p nil)
(defvar ymacs-dump-load-path nil)

(defvar ymacs-use-gnutls-p (gnutls-available-p))

(defvar ymacs-gc-cons-threshold (* 100 1024 1024))

(defvar ymacs-large-buffer-limit (* 3 1024 1024)
  "When buffer's size bigger than `ymacs-large-buffer-limit', it
will close some features to speed up emacs performance")

(defvar ymacs-file-name-handler-alist file-name-handler-alist
  "Save `file-name-handler-alist' temporarily and set it to nil
which means on every .el and .elc file loaded during start up, it
hasn't to runs those regexps against the filename.")

(defvar ymacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

(defvar ymacs-extra-config-directory
  (expand-file-name "lisp-extra" user-emacs-directory)
  "All configuration in this directory")

(defvar ymacs-var-direcotry
  (expand-file-name "var" user-emacs-directory)
  "All data and external executable file in this direcotry")

(defvar ymacs-private-directory
  (expand-file-name "private" user-emacs-directory)
  "Local packages in this directory")

(defvar ymacs-etc-direcotry
  (expand-file-name "etc" user-emacs-directory)
  "Some configuration file in this directory")

;; Add `ymacs-config-directory' to `load-path'
(add-to-list 'load-path ymacs-config-directory)

;; Package initialize occurs automatically, before `user-init-file' is
;; loaded, but after `early-init-file'. We handle package
;; initialization, so we must prevent Emacs from doing it early!
(setq package-enable-at-startup nil)

;; Inhibit resizing frame
(setq frame-inhibit-implied-resize t)

;; Faster to disable these here (before they've been initialized)
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)
(push '(undecorated . t) default-frame-alist)

;; disable native-compile during setup
(setq comp-deferred-compilation nil)

(setq load-prefer-newer t)
