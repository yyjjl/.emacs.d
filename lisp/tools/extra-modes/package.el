;;; -*- lexical-binding: t; -*-

(require-packages!
 nginx-mode
 crontab-mode
 dockerfile-mode
 yaml-mode
 csv-mode
 graphviz-dot-mode)

(after! csv-mode
  (setq csv-separators '("," ";" "|" " ")))

(after! graphviz-dot-mode
  (setq graphviz-dot-indent-width 4))
