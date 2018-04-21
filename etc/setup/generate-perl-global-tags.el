(eval-when-compile
  (require 'dash)
  (require 'cl))

(let ((default-directory (expand-var! "perl-modules")))
  (dolist (path (--filter
                 (file-exists-p it)
                 (split-string (shell-command-to-string
                                "perl -e 'for(@INC){print \"$_\n\";}'")
                               "\n"
                               :omit-nulls
                               "\n\t ")))
    (let ((link path))
      (file-name-base (directory-file-name "/usr/local/lib/x86_64-linux-gnu/perl/5.26.1"))
      (while (and link
                  (not (string-match-p
                        "perl"
                        (file-name-base (directory-file-name link)))))
        (setq link (file-name-directory (directory-file-name link))))

      (unless (equal "" link)
        (setq link (directory-file-name link))
        (when (and (> (length link) 5)
                   (member (substring link 0 5) '("/etc/" "/usr/")))
          (setq link (substring link 5)))
        (setq link (replace-regexp-in-string "/" "_" link))
        (if (file-exists-p link)
            (message "%s exists" link)
          (make-symbolic-link path link)))))
  (message "%s" (shell-command-to-string "gtags -c")))
