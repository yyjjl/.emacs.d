;;; -*- lexical-binding: t; -*-

(after! ggtags
  (setq ggtags-mode-prefix-map
        (define-key! :map (make-sparse-keymap)
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
          ("%" . ggtags-query-replace)))

  (setq ggtags-enable-navigation-keys nil)
  (setq ggtags-mode-prefix-key (kbd "C-c g"))

  (define-key! :map ggtags-mode-map
    (ggtags-mode-prefix-key :map ggtags-mode-prefix-map)
    ("M-]")
    ("M-?" . ggtags-find-reference)
    ("C-M-." . ggtags-find-tag-regexp)
    ("C-c"))

  ;; Set `ggtags-highlight-tag' to t make iedit fail to update candidates
  (setq ggtags-highlight-tag nil))
