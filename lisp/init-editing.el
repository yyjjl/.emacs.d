(setq mc/list-file "~/.emacs.d/data/mc-lists.el")

(bind-keys ("M-z" . zzz-to-char)
           ("M-Z" . zzz-up-to-char))

(bind-keys ("C-c v q" . vr/query-replace)
           ("C-c v r" . vr/replace)
           ("C-c v m" . vr/mc-mark)

           ("C-c m ," . mc/mark-next-like-this)
           ("C-c m ." . mc/mark-previous-like-this)
           ("C-c m m" . mc/mark-all-like-this)
           ("C-c m p" . mc/mark-pop)
           ("C-c m d" . mc/mark-all-like-this-dwim)

           ("C-c m l" . mc/edit-lines)
           ("C-c m e" . mc/edit-ends-of-lines)
           ("C-c m b" . mc/edit-beginnings-of-lines)

           ("C-c m n" . mc/skip-to-next-like-this)
           ("C-c m p" . mc/skip-to-previous-like-this)
           ("C-c m r" . mc/mark-all-in-region-regexp)

           ("C-c m i" . mc/insert-numbers)
           ("C-c m I" . mc/insert-letters)
           ("C-c m s" . mc/sort-regions)
           ("C-c m R" . mc/reverse-regions))

(provide 'init-editing)