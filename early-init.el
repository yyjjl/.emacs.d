;; -*- lexical-binding: t -*-

(defconst sys/win32p (eq system-type 'windows-nt)
  "Are we running on a WinTel system?")

(defconst sys/linuxp (eq system-type 'gnu/linux)
  "Are we running on a GNU/Linux system?")

(defconst sys/macp (eq system-type 'darwin)
  "Are we running on a Mac system?")

(defconst sys/mac-ns-p (eq window-system 'ns)
  "Are we running on a GNUstep or Macintosh Cocoa display?")

(defconst sys/mac-cocoa-p (featurep 'cocoa)
  "Are we running with Cocoa on a Mac system?")

(defvar ymacs-dump-load-path nil)

(defvar ymacs-gc-cons-threshold (* 100 1024 1024))

(defvar ymacs-large-buffer-limit (* 3 1024 1024)
  "When buffer's size bigger than `ymacs-large-buffer-limit', it
will close some features to speed up emacs performance")

(defconst ymacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configurations are in this directory")

(defvar ymacs-cache-direcotry
  (expand-file-name ".cache" user-emacs-directory)
  "All caches are in this direcotry")

(defconst ymacs-site-lisp-directory
  (expand-file-name "site-lisp" user-emacs-directory)
  "Local packages are in this directory")

(defconst ymacs-etc-direcotry
  (expand-file-name "etc" user-emacs-directory)
  "Some configuration files are in this directory")

(defconst ymacs-bin-direcotry
  (expand-file-name "bin" user-emacs-directory)
  "Some scripts are in this directory")

(defconst ymacs-autoloads-file (expand-file-name "autoloads.el" ymacs-cache-direcotry)
  "Autoloads file")

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
(push '(vertical-scroll-bars . nil) default-frame-alist)
(push '(undecorated . t) default-frame-alist)

;; disable native-compile during setup
;; (setq comp-deferred-compilation nil)

(setq load-prefer-newer t)

(unless (file-exists-p ymacs-cache-direcotry)
  (make-directory ymacs-cache-direcotry))
