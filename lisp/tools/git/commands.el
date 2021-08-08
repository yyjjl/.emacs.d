;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'smerge-mode)
  (require 'git-timemachine))

;;;###autoload
(defun ymacs-git/timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let* ((collection
          (mapcar
           (lambda (rev)
             (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev))
                   rev))
           (git-timemachine--revisions)))
         (key (completing-read "commits:" collection nil t)))
    (git-timemachine-show-revision (cdr (assoc-string key collection)))))

;;;###autoload
(defun ymacs-git/timemachine ()
  "Open git snapshot with the selected version."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (let ((name (buffer-name)))
    (condition-case nil
        (git-timemachine--start #'ymacs-git/timemachine-show-selected-revision)
      (quit (kill-buffer (format "timemachine:%s" name))))))

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
