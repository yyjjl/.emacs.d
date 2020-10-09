;;; -*- lexical-binding: t; -*-

(require-packages!
 doom-themes
 ;; doom-modeline
 winum)

(autoload 'doom-modeline-mode "doom-modeline" nil t)

(load-theme 'doom-molokai t)
(doom-themes-org-config)


