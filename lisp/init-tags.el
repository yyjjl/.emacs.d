(require-packages! ggtags)



(defvar-local ggtags-local-libpath nil)

(defun tags*with-local-libpath (-fn &rest -args)
  (if (stringp ggtags-local-libpath)
      (with-temp-env! (list (concat "GTAGSLIBPATH=" ggtags-local-libpath))
        (apply -fn -args))
    (apply -fn -args)))

(with-eval-after-load 'ggtags
  (advice-add 'ggtags-process-string :around #'tags*with-local-libpath)

  (setq ggtags-mode-prefix-key "\C-cg")
  ;; Set `ggtags-highlight-tag' to t make iedit fail to update
  ;; candidates
  (setq ggtags-highlight-tag nil)
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
  (define-key! :map ggtags-mode-map
    ("C-c")
    ("M-]")
    ("M-?" . ggtags-find-reference)
    ("C-M-." . ggtags-find-tag-regexp)
    (ggtags-mode-prefix-key :map ggtags-mode-prefix-map)))

(provide 'init-tags)
