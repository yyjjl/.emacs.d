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

(global-set-key (kbd "M--") 'er/expand-region)

(with-eval-after-load 'fcitx
  ;; init fcitx prefix keys
  (setq fcitx-use-dbus t)
  (fcitx-prefix-keys-add "C-h" "M-g"))

(with-eval-after-load 'picture
  (defhydra hydra-picture-move (:pre (setq hydra-is-helpful nil) ;; do not show lv
                             :post (setq hydra-is-helpful t))
    "move"
    ("n" picture-move-down)
    ("p" picture-move-up)
    ("f" picture-motion)
    ("b" picture-motion-reverse)

    ("C-SPC" set-mark-command)
    ("RET" nil nil))
  (bind-keys :map picture-mode-map
             ("C-d" . picture-delete-char)
             ("C-c C-f") ("C-c C-b")
             ("C-c a" . artist-mode)
             ("C-f" . hydra-picture-move/picture-motion)
             ("C-b" . hydra-picture-move/picture-motion-reverse)
             ("C-n" . hydra-picture-move/picture-move-down)
             ("C-p" . hydra-picture-move/picture-move-up)))

(with-eval-after-load 'artist
  (bind-keys :map artist-mode-map
             ("p" . artist-previous-line)
             ("n" . artist-next-line)
             ("b" . artist-backward-char)
             ("f" . artist-forward-char)

             ("C-c a e" . artist-select-erase-char)
             ("C-c a f" . artist-select-fill-char)
             ("C-c a l" . artist-select-line-char)
             ("C-c a o" . artist-select-operation)
             ("C-c a r" . artist-toggle-rubber-banding)
             ("C-c a t" . artist-toggle-trim-line-endings)
             ("C-c a s" . artist-toggle-borderless-shapes)
             ("C-c s l"    . artist-select-op-line)
             ("C-c s L"    . artist-select-op-straight-line)
             ("C-c s r"    . artist-select-op-rectangle)
             ("C-c s R"    . artist-select-op-square)
             ("C-c s s"    . artist-select-op-square)
             ("C-c s p"    . artist-select-op-poly-line)
             ("C-c s P"    . artist-select-op-straight-poly-line)
             ("C-c s e"    . artist-select-op-ellipse)
             ("C-c s c"    . artist-select-op-circle)
             ("C-c s t"    . artist-select-op-text-see-thru)
             ("C-c s T"    . artist-select-op-text-overwrite)
             ("C-c s S"    . artist-select-op-spray-can)
             ("C-c s z"    . artist-select-op-spray-set-size)
             ("C-c s d" . artist-select-op-erase-char)
             ("C-c s E"    . artist-select-op-erase-rectangle)
             ("C-c s v"    . artist-select-op-vaporize-line)
             ("C-c s V"    . artist-select-op-vaporize-lines)
             ("C-c s k" . artist-select-op-cut-rectangle)
             ("C-c s w" . artist-select-op-copy-rectangle)
             ("C-c s y" . artist-select-op-paste)
             ("C-c s f"    . artist-select-op-flood-fill)))

(provide 'init-editing)
