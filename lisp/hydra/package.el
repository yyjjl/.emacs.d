;;; -*- lexical-binding: t; -*-

(require-packages! hydra pretty-hydra)

(defvar ymacs-hydra-local-toggles-heads-list nil)

(autoload 'ymacs-hydra/ediff/body "hydra/commands" nil t)
(autoload 'ymacs-hydra/global-toggles/body "hydra/commands" nil t)
(autoload 'ymacs-hydra/next-error/body "hydra/commands" nil t)
(autoload 'ymacs-hydra/outline/body "hydra/commands" nil t)
(autoload 'ymacs-hydra/rectangle/body "hydra/commands" nil t)
(autoload 'ymacs-hydra/window/enlarge-window "hydra/commands" nil t)
(autoload 'ymacs-hydra/window/enlarge-window-horizontally "hydra/commands" nil t)
(autoload 'ymacs-hydra/window/shrink-window "hydra/commands" nil t)
(autoload 'ymacs-hydra/window/shrink-window-horizontally "hydra/commands" nil t)
(autoload 'ymacs-hydra/sort/body "hydra/commands" nil t)
(autoload 'ymacs-hydra/games/body "hydra/commands" nil t)

(define-key!
  ("C-x {" . ymacs-hydra/window/shrink-window-horizontally)
  ("C-x }" . ymacs-hydra/window/enlarge-window-horizontally)
  ("C-x ^" . ymacs-hydra/window/enlarge-window)
  ("C-x -" . ymacs-hydra/window/shrink-window)
  ("C-x ?" . ymacs-hydra/window/body)
  ("C-x , z" . ymacs-hydra/games/body)

  ([M-f11] . scroll-other-window-down)
  ([M-f12] . scroll-other-window)

  ([f7] . ymacs-hydra/global-toggles/body)

  ("C-c O" . ymacs-hydra/outline/body)
  ("C-x SPC" . ymacs-hydra/rectangle/body)

  ("C-x , s" . ymacs-hydra/sort/body)
  ("C-x , e" . ymacs-hydra/ediff/body)

  ("C-x `" . ymacs-hydra/next-error/body))
