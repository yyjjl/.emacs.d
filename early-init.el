;; -*- lexical-binding: t -*-

(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

(defvar emacs-var-direcotry
  (expand-file-name "var" user-emacs-directory)
  "All data and external executable file in this direcotry")

(defvar emacs-lite-setup-p (getenv "EMACS_LITE"))

(defvar emacs-gc-cons-threshold (* 10 1024 1024))

(defvar emacs-file-name-handler-alist file-name-handler-alist
  "Save `file-name-handler-alist' temporarily and set it to nil
which means on every .el and .elc file loaded during start up, it
hasn't to runs those regexps against the filename.")

(defvar emacs-private-directory
  (expand-file-name "private" user-emacs-directory)
  "Local packages in this directory")

(defvar emacs-use-gnutls-p (gnutls-available-p)
  "Use gnutls to download packages")

(defvar emacs-etc-direcotry
  (expand-file-name "etc" user-emacs-directory)
  "Some configuration file in this directory")

(defvar emacs-autoloads-directory
  (expand-file-name "autoloads" emacs-config-directory)
  "Autoloads files in this directory")

;; Add `emacs-config-directory' to `load-path'
(add-to-list 'load-path emacs-config-directory)

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
