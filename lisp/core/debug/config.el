;;; -*- lexical-binding: t; -*-

(after! gdb-mi
  ;; we restore configuration manually
  (setq gdb-restore-window-configuration-after-quit nil)
  (setq gdb-show-main t)
  (setq gdb-many-windows nil))
