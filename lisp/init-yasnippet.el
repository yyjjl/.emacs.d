(with-eval-after-load 'yasnippet
  (defun my-yas-reload-all ()
    (interactive)
    (yas-compile-directory (file-truename "~/.emacs.d/snippets"))
    (yas-reload-all))

  (yas-reload-all)

  (add-to-list 'auto-mode-alist '("\\.yasnippet\\'" . snippet-mode))

  (setq-default mode-require-final-newline nil)
  (setq yas-prompt-functions '(yas-completing-prompt
                               yas-ido-prompt))

  ;; use yas-completing-prompt when ONLY when `M-x yas-insert-snippet'
  ;; thanks to capitaomorte for providing the trick.
  (defadvice yas-insert-snippet (around use-completing-prompt activate)
    "Use `yas-completing-prompt' for `yas-prompt-functions' but only here..."
    (let ((yas-prompt-functions '(yas-completing-prompt)))
      ad-do-it)))

(defun yasnippet-generic-setup-for-mode-hook ()
  (unless (is-buffer-file-temp)
    ;; highlight FIXME/BUG/TODO in comment
    (yas-minor-mode 1)))

(add-hook 'prog-mode-hook
          'yasnippet-generic-setup-for-mode-hook)
(add-hook 'text-mode-hook 'yasnippet-generic-setup-for-mode-hook)
(add-hook 'cmake-mode-hook 'yasnippet-generic-setup-for-mode-hook)

(provide 'init-yasnippet)
