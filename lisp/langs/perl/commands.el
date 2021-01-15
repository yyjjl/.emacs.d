;; -*- lexical-binding:t -*-

(defun ymacs-perl//format-region (-beg -end)
  (unless (and ymacs-perltidy-path
               (file-executable-p ymacs-perltidy-path))
    (user-error "perltidy can not be found"))

  (unless (char-equal ?\n (char-before -end))
    (setq -end (min (save-excursion ;; must including terminating newline
                      (goto-char -end)
                      (1+ (line-end-position)))
                    (point-max))))
  (let ((old-point (point)))
    (apply #'call-process-region
           -beg -end
           ymacs-perltidy-path t '(t nil)
           "--quiet"
           "--standard-error-output"
           ymacs-perl-perltidy-options)
    (goto-char old-point)))

;;;###autoload
(defun ymacs-perl/perltidy-format (&optional -arg)
  "Format Perl code with perltidy.
If region is active, operate on it, else operate on line."
  (interactive "p")
  (if (use-region-p)
      (ymacs-perl//format-region (region-beginning) (region-end))
    (cond
     ((eq -arg 1)
      (ymacs-perl//format-region (save-excursion (beginning-of-defun) (point))
                                 (save-excursion (end-of-defun) (point))))
     ((eq -arg 4)
      (ymacs-perl//format-region (point-min) (point-max))))))

;;;###autoload
(defun ymacs-perl/generate-global-tags ()
  (interactive)
  (let ((default-directory (expand-cache! "perl-modules"))
        (lib-paths
         (shell-command-to-string "perl -e 'for(@INC){print \"$_\n\";}'")))
    (dolist (path (--filter
                   (file-exists-p it)
                   (split-string lib-paths "\n" :omit-nulls "\n\t ")))
      (let ((link path))
        (while (and link
                    (not (string-match-p
                          "perl"
                          (file-name-base (directory-file-name link)))))
          (setq link (file-name-directory (directory-file-name link))))

        (unless (string-empty-p link)
          (setq link (directory-file-name link))
          (when (and (> (length link) 5)
                     (member (substring link 0 5) '("/etc/" "/usr/")))
            (setq link (substring link 5)))
          (setq link (replace-regexp-in-string "/" "_" link))
          (if (file-exists-p link)
              (message "%s exists" link)
            (make-symbolic-link path link)))))
    (run-compilation!
     :-name "Generate perl tags"
     :-command "gtags -cv --statistics")))
