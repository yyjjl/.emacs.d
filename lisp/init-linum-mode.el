(package|require 'nlinum)



(with-eval-after-load 'nlinum
  (setq main|linum-disabled-modes
        '(org-mode))
  (setq nlinum-highlight-current-line t)

  (defun main|disable-linum-mode (fn &rest args)
    (when (and (not (memq major-mode main|linum-disabled-modes))
               (or (derived-mode-p 'prog-mode)
                   (derived-mode-p 'text-mode))
               (< (buffer-size) main|large-buffer-size))
      (apply fn args)))
  (advice-add 'nlinum-mode :around #'main|disable-linum-mode))
;; Slow down emacs
(global-nlinum-mode 1)

(provide 'init-linum-mode)
