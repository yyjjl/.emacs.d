;; -*- lexical-binding: t; -*-

(after! pdf-view
  (add-to-list
   'shackle-rules
   '(pdf-outline-buffer-mode :align left :size 40 :select nil)))
