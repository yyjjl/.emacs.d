(add-hook 'sh-mode-hook
          '(lambda ()
             (unless (buffer-temporary-p)
               (add-to-list 'company-backends 'company-files)
               (flycheck-mode 1))))


;; But don't show trailing whitespace in SQLi, inf-ruby etc.
(add-hook 'comint-mode-hook
          (lambda () (setq show-trailing-whitespace nil)))

(with-eval-after-load 'sh-script
  ;; Fix freeze problem
  (defun sh-smie-sh-forward-token ()
    (if (and (looking-at "[ \t]*\\(?:#\\|\\(\\s|\\)\\|$\\)")
            (save-excursion
              (skip-chars-backward " \t")
              (not (bolp))))
        (if (and (match-end 1) (not (nth 3 (syntax-ppss))))
            ;; Right before a here-doc.
            (let ((forward-sexp-function nil))
              (forward-sexp 1)
              ;; Pretend the here-document is a "newline representing a
              ;; semi-colon", since the here-doc otherwise covers the newline(s).
              ";")
          (unless (eobp)
            (let ((semi (sh-smie--newline-semi-p)))
              (forward-line 1)
              (if (or semi (eobp))
                  ";"
                (sh-smie-sh-forward-token)))))
      (forward-comment (point-max))
      (cond
       ((looking-at "\\\\\n") (forward-line 1) (sh-smie-sh-forward-token))
       ((looking-at sh-smie--sh-operators-re)
        (goto-char (match-end 0))
        (let ((tok (match-string-no-properties 0)))
          (if (and (memq (aref tok (1- (length tok))) '(?\; ?\& ?\|))
                  (looking-at "[ \t]*\\(?:#\\|$\\)"))
              (forward-line 1))
          tok))
       (t
        (let* ((pos (point))
               (tok (sh-smie--default-forward-token)))
          (cond
           ((equal tok ")") "case-)")
           ((equal tok "(") "case-(")
           ((and tok (string-match "\\`[a-z]" tok)
                (assoc tok smie-grammar)
                (not
                 (save-excursion
                   (goto-char pos)
                   (sh-smie--sh-keyword-p tok))))
            " word ")
           (t tok))))))))

(provide 'init-sh)
