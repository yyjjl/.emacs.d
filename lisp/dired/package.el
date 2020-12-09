;;; -*- lexical-binding: t; -*-

(executable! gls)

(require-packages!
 (dired-quick-sort :when (or (not sys/macp) ymacs-gls-path)))

