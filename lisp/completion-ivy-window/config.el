;;; -*- lexical-binding: t; -*-

(after! winum
  (cl-pushnew ymacs-ivy--candidate-buffer winum-ignored-buffers))

(after! ivy
  (setq ivy-display-functions-alist
        '((counsel-irony . ivy-display-function-overlay)
          (ivy-completion-in-region . ivy-display-function-overlay)
          (t . ymacs-ivy/display-function-window))))

