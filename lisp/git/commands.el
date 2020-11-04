;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-git//status ()
  (interactive)
  (when (file-remote-p default-directory)
    (user-error "Magit is disabled on remote host"))
  (call-interactively #'magit-status))

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

;;;###autoload(autoload 'ymacs-hydra/smerge/body "git/commands" nil t)
(pretty-hydra-define ymacs-hydra/smerge
  (:title "Smerge" :color pink :quit-key "q")
  ("Move"
   (("n" smerge-next "next hunk")
    ("p" smerge-prev "prev hunk"))
   "Keep"
   (("b" smerge-keep-base "keep base")
    ("u" smerge-keep-upper "keep upper")
    ("l" smerge-keep-lower "keep lower")
    ("a" smerge-keep-all "keep all")
    ("RET" smerge-keep-current "keep current")
    ("C-m" smerge-keep-current "keep current"))
   "Diff"
   (("<" smerge-diff-base-upper "base/upper")
    ("=" smerge-diff-upper-lower "upper/lower")
    (">" smerge-diff-base-lower "base/lower")
    ("R" smerge-refine "refine")
    ("E" smerge-ediff "ediff"))
   "Other"
   (("C" smerge-combine-with-next "combine")
    ("r" smerge-resolve "resolve")
    ("k" smerge-kill-current "kill")
    ("ZZ" (lambda ()
            (interactive)
            (save-buffer)
            (bury-buffer))
     "Save and bury buffer" :exit t))))
