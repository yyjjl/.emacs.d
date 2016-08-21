;; Colourise CSS colour literals
;; web-mode does not like rainbow-mode

(defun my-css-imenu-make-index ()
  (save-excursion
    (imenu--generic-function '((nil "^ *\\([^ ]+\\) *{ *$" 1)
                               ))))


(dolist (hook '(css-mode-hook sass-mode-hook))
  (add-hook hook
            '(lambda ()
               (rainbow-mode 1)
               (flycheck-mode 1)
               (unless (is-buffer-file-temp)
                 (setq imenu-create-index-function 'my-css-imenu-make-index)))))

(provide 'init-css)
