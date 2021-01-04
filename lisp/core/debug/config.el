;;; -*- lexical-binding: t; -*-

(after! ace-window
  (add-to-list 'aw-ignored-buffers ymacs-debug--help-buffer-name))

(after! gdb-mi
  (setq gdb-show-main t)
  (setq gdb-many-windows nil))
