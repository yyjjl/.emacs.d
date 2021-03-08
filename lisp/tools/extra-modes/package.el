;;; -*- lexical-binding: t; -*-

(require-packages!
 nginx-mode
 bison-mode
 gnuplot-mode
 crontab-mode
 dockerfile-mode
 yaml-mode
 csv-mode
 graphviz-dot-mode
 zeal-at-point)

(define-key! :prefix "C-h"
  ("z" . zeal-at-point)
  ("Z" . zeal-at-point-search))

(after! csv-mode
  (setq csv-separators '("," ";" "|" " ")))

(after! zeal-at-point
  (setf (cdr (assoc 'c++-mode zeal-at-point-mode-alist)) "cpp"
        (cdr (assoc 'python-mode zeal-at-point-mode-alist)) "python")
  (add-to-list 'zeal-at-point-mode-alist '(cmake-mode . "cmake")))

(after! graphviz-dot-mode
  (require 'company-graphviz-dot)

  (remove-hook 'company-backends 'company-graphviz-dot-backend)
  (define-hook! ymacs-editor//dot-setup (graphviz-dot-mode-hook)
    (ymacs-editor//add-company-backend 'company-graphviz-dot-backend))

  (setq graphviz-dot-indent-width 4))
