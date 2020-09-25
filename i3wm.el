;; -*- lexical-binding:t -*-

(load (expand-file-name "early-init" user-emacs-directory))

(setq ymacs-var-direcotry (expand-file-name "var/i3" user-emacs-directory))

(add-to-list 'load-path ymacs-config-directory)
(add-to-list 'load-path (expand-file-name "etc/i3wm" user-emacs-directory))

(require 'i3wm-init)
(require 'i3wm-doc)

(setq-default default-frame-alist
              `((name . "Minibuffer-i3wm")
                (width . 100)
                (height . ,(+ 3 ivy-height))
                (menu-bar-lines . 0)
                (tool-bar-lines . 0)
                (vertical-scroll-bars . nil)
                (unsplittable . t)))

;; Indexing documents
(add-hook 'midnight-hook 'i3//index-document-files)
(i3//index-document-files)

(run-with-idle-timer
 10 t
 (lambda ()
   (recentf-save-list)
   (session-save-session)))
