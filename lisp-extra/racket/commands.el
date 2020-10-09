;; -*- lexical-binding:t -*-

;;;###autoload
(defun ymacs-lisp/racket-indent-sexp ()
  (interactive)
  (unless (region-active-p)
    (lispy-mark-list 1))
  (call-interactively #'indent-region)
  (lispy-different))

;;;###autoload
(defun ymacs-lisp/racket-eval-sexp ()
  (interactive)
  (unless (region-active-p)
    (lispy-mark-list 1))
  (call-interactively #'racket-send-region))
