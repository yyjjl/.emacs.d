(require-packages! ggtags)

(defvar-local ggtags-local-libpath nil)

(config! ggtags
  :bind
  (:map (make-sparse-keymap)
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
  (:map ggtags-mode-map
   ("C-c")
   ("M-]")
   ("M-?" . ggtags-find-reference)
   ("C-M-." . ggtags-find-tag-regexp)
   (ggtags-mode-prefix-key :map ggtags-mode-prefix-map))

  :advice
  (:around ggtags-process-string
   :define (-fn &rest -args)
   (if (stringp ggtags-local-libpath)
       (with-temp-env! (list (concat "GTAGSLIBPATH=" ggtags-local-libpath))
         (apply -fn -args))
     (apply -fn -args)))

  :config
  (setq ggtags-mode-prefix-key "\C-cg")
  ;; Set `ggtags-highlight-tag' to t make iedit fail to update candidates
  (setq ggtags-highlight-tag nil))

(provide 'init-tags)
