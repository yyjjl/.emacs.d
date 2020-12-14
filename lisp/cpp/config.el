;; -*- lexical-binding:t -*-

(cond ((eq font-lock-maximum-decoration t)
       (setq font-lock-maximum-decoration
             '((c++-mode . 1) (c-mode . 1) (t . t))))
      ((listp font-lock-maximum-decoration)
       (dolist (mode '(c++-mode c-mode))
         (if-let (cell (assoc mode font-lock-maximum-decoration))
             (setcdr cell 1)
           (push (cons mode 1) font-lock-maximum-decoration)))))

(after! cc-mode
  (define-key! :map c-mode-base-map
    ("*" . ymacs-cpp/electric-star)
    ("C-c o" . ff-find-other-file)
    ("C-c C-j" . semantic-ia-fast-jump)
    ("C-c C-v" . semantic-ia-show-variants)
    ("M-n" . next-error)
    ("M-p" . previous-error)
    ("C-M-i" . counsel-company))

  (dolist (map (list c-mode-map c++-mode-map))
    (define-key! :map map
      ("<")
      (">")
      ("C-c C-d")
      (("C-c C-b" "C-C b") . clang-format-buffer)
      ("C-c C-e" . ymacs-cpp/macro-expand)
      ("C-c C-l" . ymacs-cpp/load-in-repl)
      ([f10] . ymacs-cpp/compile)
      ([f5] . ymacs-cpp/debug-current-file)))

  (dolist (key '("#" "}" "/" ";" "," ":" "(" ")" "{"))
    (define-key c-mode-base-map key nil)))
