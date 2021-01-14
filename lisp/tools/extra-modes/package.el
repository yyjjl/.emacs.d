;;; -*- lexical-binding: t; -*-

(require-packages!
 nginx-mode
 bison-mode
 gnuplot-mode
 crontab-mode
 dockerfile-mode
 yaml-mode
 csv-mode
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
