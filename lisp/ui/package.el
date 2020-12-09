;;; -*- lexical-binding: t; -*-

(require-packages!
 hl-todo
 page-break-lines
 doom-themes
 which-key
 winum
 highlight-indent-guides)

(autoload 'doom-modeline-mode "doom-modeline" nil t)

(load-theme 'doom-molokai t)
(doom-themes-org-config)

(defvar ymacs-ui-view-code-modes
  '((t display-line-numbers-mode
       view-mode
       highlight-indent-guides-mode)))

(define-key!
  ("C-x , ," . ymacs-ui/view-code-mode))


