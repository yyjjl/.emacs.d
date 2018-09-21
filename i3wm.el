(defvar emacs-config-directory
  (expand-file-name "lisp" user-emacs-directory)
  "All configuration in this directory")

(defvar emacs-i3wm-config-directory
  (expand-file-name "etc/i3wm" user-emacs-directory)
  "All i3wm configuration in this directory")

(defvar emacs-var-direcotry
  (expand-file-name "var/i3" user-emacs-directory)
  "All data and external executable file in this direcotry")

(add-to-list 'load-path emacs-config-directory)
(add-to-list 'load-path emacs-i3wm-config-directory)

(require 'i3wm-init)
(require 'i3wm-doc)

(setq-default default-frame-alist
              `((name . "Minibuffer-i3wm")
                (width . 100)
                (height . ,(+ 4 ivy-height))
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
