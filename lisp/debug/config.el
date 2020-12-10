;;; -*- lexical-binding: t; -*-

(after! winum
  (add-to-list 'winum-ignored-buffers ymacs-debug--help-buffer-name))

(after! gdb-mi
  (setq gdb-show-main t)
  (setq gdb-many-windows nil))
