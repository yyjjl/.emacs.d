(package|require 'ggtags)



(with-eval-after-load 'ggtags
  (setq gtags-suggested-key-mapping t)
  (setq ggtags-mode-prefix-key "\C-cg")
  (setq ggtags-mode-prefix-map
        (let ((m (make-sparse-keymap)))
          (define-keys :map m
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
  (define-key ggtags-mode-map ggtags-mode-prefix-key ggtags-mode-prefix-map))

(provide 'init-tags)
