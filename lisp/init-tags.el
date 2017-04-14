(with-eval-after-load 'ggtags
  (setq gtags-suggested-key-mapping t)
  (setq ggtags-mode-prefix-key "\C-cg")
  (setq ggtags-mode-prefix-map
        (let ((m (make-sparse-keymap)))
          (bind-keys :map m
                     ("D" . ggtags-delete-tags)
                     ("p" . ggtags-prev-mark)
                     ("n" . ggtags-next-mark)
                     ("f" . ggtags-find-file)
                     ("o" . ggtags-find-other-symbol)
                     ("g" . ggtags-grep)
                     ("i" . ggtags-idutils-query)
                     ("b" . ggtags-browse-file-as-hypertext)
                     ("k" . ggtags-kill-file-buffers)
                     ("h" . ggtags-view-tag-history)
                     ("j" . ggtags-visit-project-root)
                     ("/" . ggtags-view-search-history)
                     ("SPC" . ggtags-save-to-register)
                     ("%" . ggtags-query-replace))
          m))
  (define-key ggtags-mode-map "\C-c" nil)
  (define-key ggtags-mode-map "\M-?" 'ggtags-show-definition)
  (define-key ggtags-mode-map ggtags-mode-prefix-key ggtags-mode-prefix-map))


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
