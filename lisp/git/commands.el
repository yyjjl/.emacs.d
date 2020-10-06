;;; -*- lexical-binding: t; -*-

(defun ymacs-git//format-hunk (-ov)
  (let ((start (overlay-start -ov)))
    (cons
     (format "%6d ~ %-6d: %s"
             start
             (overlay-end -ov)
             (save-excursion
               (goto-char start)
               (buffer-substring (line-beginning-position)
                                 (line-end-position))))
     -ov)))

;;;###autoload
(defun ymacs-git/goto-hunk ()
  (interactive)
  (let ((hunks
         (-->
          (overlays-in (point-min) (point-max))
          (cl-remove-if-not (lambda (x) (overlay-get x 'diff-hl-hunk)) it)
          (mapcar #'ymacs-git//format-hunk it)
          (sort it (lambda (x y)
                     (< (overlay-start (cdr x))
                        (overlay-start (cdr y))))))))
    (ivy-read
     "hunks: " hunks
     :require-match t
     :action (lambda (x) (goto-char (overlay-start (cdr x))))
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
