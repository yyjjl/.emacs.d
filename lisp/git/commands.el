;;; -*- lexical-binding: t; -*-

(defun ymacs-git//format-gutter (-gutter)
  (let ((start-line (aref -gutter 3)))
    (cons
     (format "%s %d ~ %d: %s"
             (pcase (aref -gutter 1)
               (`added "+")
               (`deleted "-")
               (`modified "="))
             start-line
             (aref -gutter 4)
             (ignore-errors
               (let ((string (nth 1 (split-string (aref -gutter 2) "\n" t))))
                 (if (string-match-p "^-\\|\\+\\|=" string)
                     (substring string 1)
                   string))))
     start-line)))

;;;###autoload
(defun ymacs-git/goto-gutter ()
  (interactive)
  (unless git-gutter:diffinfos
    (user-error "No git-gutters!"))
  (let ((gutters (mapcar #'ymacs-git//format-gutter git-gutter:diffinfos)))
    (ivy-read "git-gutters:" gutters
              :require-match t
              :action (lambda (x)
                        (forward-line (- (cdr x) (line-number-at-pos))))
              :keymap (define-key! :map (make-sparse-keymap)
                        ("C-n" . ivy-next-line-and-call)
                        ("C-p" . ivy-previous-line-and-call)))))

;;;###autoload
(defun ymacs-git/timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar
           (lambda (rev)
             ;; re-shape list for the ivy-read
             (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev))
                   rev))
           (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :action (lambda (rev) (git-timemachine-show-revision (cdr rev))))))

;;;###autoload
(defun ymacs-git/timemachine ()
  "Open git snapshot with the selected version.  Based on `ivy-mode'."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (let ((name (buffer-name)))
    (condition-case nil
        (git-timemachine--start #'ymacs-git/timemachine-show-selected-revision)
      (quit (kill-buffer (format "timemachine:%s" name))))))
