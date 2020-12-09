;;; -*- lexical-binding: t; -*-

(after! treemacs
  (define-advice winum-select-window-by-number
      (:around (-fn &optional -arg) switch-between-window-1-and-treemacs)
    "Jump to window 1 or treemacs-window."
    (interactive)
    (if (and (eq major-mode 'treemacs-mode)
             (integerp -arg)
             (> -arg 1)
             (not (winum-get-window-by-number -arg)))
        (funcall -fn (1- -arg))
      (funcall -fn -arg))))
