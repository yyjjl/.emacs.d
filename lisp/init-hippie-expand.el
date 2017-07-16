(with-eval-after-load 'hippie-exp
  ;; @ see http://blog.binchen.org/posts/autocomplete-with-a-dictionary-with-hippie-expand.html
  (defun try-expand-by-dict (old)
    ;; old is true if we have already attempted an expansion
    (unless (bound-and-true-p ispell-minor-mode)
      (ispell-minor-mode 1))

    (let ((lookup-func (if (fboundp 'ispell-lookup-words)
                           'ispell-lookup-words
                         'lookup-words)))
      (unless old
        (he-init-string (he-lisp-symbol-beg) (point))
        (if (not (he-string-member he-search-string he-tried-table))
            (setq he-tried-table (cons he-search-string he-tried-table)))
        (setq he-expand-list
              (and (not (equal he-search-string ""))
                  (funcall lookup-func (concat (buffer-substring-no-properties (he-lisp-symbol-beg) (point)) "*")))))
      (if (null he-expand-list)
          (if old (he-reset-string))
        (he-substitute-string (car he-expand-list))
        (setq he-expand-list (cdr he-expand-list))
        t)
      ))
  (setq hippie-expand-try-functions-list
        '(try-complete-file-name-partially
          try-complete-file-name
          try-expand-dabbrev
          try-expand-dabbrev-all-buffers
          try-expand-dabbrev-from-kill
          try-expand-by-dict)))

(provide 'init-hippie-expand)
