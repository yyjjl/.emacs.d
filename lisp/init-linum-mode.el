(require! 'nlinum)



(with-eval-after-load 'nlinum
  (setq core/linum-disabled-modes
        '(org-mode))
  (setq nlinum-highlight-current-line t)

  (defun core*disable-linum-mode ($fn &rest $args)
    (when (and (not (memq major-mode core/linum-disabled-modes))
               (or (derived-mode-p 'prog-mode)
                   (derived-mode-p 'text-mode))
               (< (buffer-size) core-large-buffer-size))
      (apply $fn $args)))
  (advice-add 'nlinum-mode :around #'core*disable-linum-mode))

(defalias 'linum-mode 'nlinum-mode)
(global-nlinum-mode 1)

(provide 'init-linum-mode)
