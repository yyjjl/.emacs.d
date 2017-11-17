(require-packages! ggtags)



(with-eval-after-load 'ggtags
  (setq ggtags-mode-prefix-key "\C-cg"
        ;; Set `ggtags-highlight-tag' to t make iedit fail to update
        ;; candidates
        ggtags-highlight-tag nil
        ggtags-mode-prefix-map
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
  (define-key ggtags-mode-map "\C-c" nil)
  (define-key ggtags-mode-map (kbd "M-]") nil)
  (define-key ggtags-mode-map ggtags-mode-prefix-key ggtags-mode-prefix-map))

(provide 'init-tags)
