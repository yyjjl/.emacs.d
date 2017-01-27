(with-eval-after-load 'ggtags
  (setq gtags-suggested-key-mapping t)
  (bind-keys :map ggtags-mode-map
             ( "C-c g d" . ggtags-find-definition)
             ( "C-c g o" . ggtags-find-other-symbol)
             ( "C-c g r" . ggtags-find-tag-regexp)
             ( "C-c g f" . ggtags-find-file)
             ( "C-c g q" . ggtags-query-replace)
             ( "C-c g c" . ggtags-create-tags)
             ( "C-c g u" . ggtags-update-tags)
             ( "C-c g D" . ggtags-delete-tags)
             ( "C-c g h" . ggtags-view-tag-history)
             ("M-?" . ggtags-show-definition)))


(defun ggtags-auto-update ()
  (condition-case nil
      (ggtags-update-tags)
    (error nil)))

(define-minor-mode ggtags-auto-update-mode
  "auto update TAGS using `exuberant-ctags' in parent directory."
  ;; :global t
  :init-value nil
  :group 'ggtags
  (if ggtags-auto-update-mode
      (progn
        (add-hook 'after-save-hook 'ggtags-auto-update nil t)
        (run-hooks 'ggtags-auto-update-mode-hook))
    (remove-hook 'after-save-hook 'ggtags-auto-update t)))

(provide 'init-tags)
