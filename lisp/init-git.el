;; @see http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
;; {{ git gutter with ivy
(defun my-reshape-git-gutter (gutter)
  "Re-shape gutter for `ivy-read'."
  (let* ((linenum-start (aref gutter 3))
         (linenum-end (aref gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      ;; find out the longest stripped line in the gutter
      (while (<= linenum-start linenum-end)
        (goto-line linenum-start)
        (setq tmp-line (replace-regexp-in-string "^[ \t]*" ""
                                                 (buffer-substring (line-beginning-position)
                                                                   (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

(defun my-goto-git-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((collection (mapcar 'my-reshape-git-gutter
                                 git-gutter:diffinfos)))
        (ivy-read "git-gutters:"
                  collection
                  :action (lambda (linenum)
                            (goto-line linenum))))
    (message "NO git-gutters!")))
;; }}

;; @see http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
;; {{ git-timemachine
(defun my-git-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|" (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun my-git-timemachine ()
  "Open git snapshot with the selected version.  Based on ivy-mode."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'my-git-timemachine-show-selected-revision))
;; }}

(bind-keys ("C-x g h" . git-gutter:popup-hunk)
           ("C-x g s" . git-gutter:stage-hunk)
           ("C-x g r" . git-gutter:revert-hunk)
           ("C-x g t" . my-git-timemachine)
           ("C-x n" . git-gutter:next-hunk)
           ("C-x p" . git-gutter:previous-hunk)
           ("C-x g g" . my-goto-git-gutter)
           ("C-x g l" . git-link)
           ("C-x g c" . git-link-commit)
           ("C-x g m" . git-messenger:popup-message))

(with-eval-after-load 'git-gutter
  (require 'git-gutter-fringe)
  (require 'fringe-helper)
  (set-face-foreground 'git-gutter-fr:modified "yellow")
  (set-face-foreground 'git-gutter-fr:added    "green")
  (set-face-foreground 'git-gutter-fr:deleted  "red")
  (fringe-helper-define 'git-gutter-fr:deleted nil
    "........"
    "........"
    "........"
    "XXXXXXXX"
    "XXXXXXXX"
    "........"
    "........"
    "........")

  (fringe-helper-define 'git-gutter-fr:added nil
    "...XX..."
    "...XX..."
    "...XX..."
    "XXXXXXXX"
    "XXXXXXXX"
    "...XX..."
    "...XX..."
    "...XX...")

  (fringe-helper-define 'git-gutter-fr:modified nil
    "........"
    "XXXXXXXX"
    "XXXXXXXX"
    "........"
    "........"
    "XXXXXXXX"
    "XXXXXXXX"
    "........")
  (setq git-gutter:handled-backends '(svn hg git)
        git-gutter-fr:side 'right-fringe))

(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t)
  (add-hook 'git-messenger:after-popup-hook
            (lambda (msg)
              ;; extract commit id and put into the kill ring
              (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
                (copy-yank-str (match-string 2 msg))
                (message "commit hash %s => clipboard & kill-ring" (match-string 2 msg))
                ))) )


(provide 'init-git)

