(autoload 'hydra-resize-window/shrink-window "autoloads/hydra" nil t)
(autoload 'hydra-resize-window/shrink-window-horizontally "autoloads/hydra" nil t)
(autoload 'hydra-resize-window/enlarge-window "autoloads/hydra" nil t)
(autoload 'hydra-resize-window/enlarge-window-horizontally "autoloads/hydra" nil t)

(autoload 'hydra-move-invoker "autoloads/hydra" nil t)

(autoload 'hydra-global-toggles/body "autoloads/hydra" nil t)

(autoload 'scroll-down-line-other-window "autoloads/hydra" nil t)
(autoload 'scroll-up-line-other-window "autoloads/hydra" nil t)

(autoload 'hydra-outline/body "autoloads/hydra" nil t)
(autoload 'hydra-rectangle/body "autoloads/hydra" nil t)
(autoload 'hydra-sort/body "autoloads/hydra" nil t)
(autoload 'hydra-next-error/body "autoloads/hydra" nil t)

(define-key!
  ("C-x {" . hydra-resize-window/shrink-window-horizontally)
  ("C-x }" . hydra-resize-window/enlarge-window-horizontally)
  ("C-x ^" . hydra-resize-window/enlarge-window)
  ("C-x -" . hydra-resize-window/shrink-window)

  ("C->" . hydra-move-invoker)
  ("M-N" . scroll-down-line-other-window)
  ("M-P" . scroll-up-line-other-window)

  ([M-f11] . scroll-other-window-down)
  ([M-f12] . scroll-other-window)

  ([f7] . hydra-global-toggles/body)

  ("C-c O" . hydra-outline/body)
  ("C-x SPC" . hydra-rectangle/body)
  ("C-x , s" . hydra-sort/body)
  ("C-x `" . hydra-next-error/body))

(provide 'core-hydra)
