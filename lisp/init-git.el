(require-packages!
 gitignore-mode
 gitconfig-mode
 git-messenger
 git-gutter
 git-gutter-fringe
 git-link
 git-timemachine
 magit)



(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read)
  (setq vc-handled-backends nil)

  (require 'magit-autorevert))

;; @see
;; http://blog.binchen.org/posts/enhance-emacs-git-gutter-with-ivy-mode.html
;; {{ git gutter with ivy
(defun git/reshape-gutter ($gutter)
  "Re-shape GUTTER for `ivy-read'."
  (let* ((linenum-start (aref $gutter 3))
         (linenum-end (aref $gutter 4))
         (target-line "")
         (target-linenum 1)
         (tmp-line "")
         (max-line-length 0))
    (save-excursion
      ;; find out the longest stripped line in the $gutter
      (while (<= linenum-start linenum-end)
        (forward-line 1)
        (setq tmp-line (replace-regexp-in-string
                        "^[ \t]*" ""
                        (buffer-substring (line-beginning-position)
                                          (line-end-position))))
        (when (> (length tmp-line) max-line-length)
          (setq target-linenum linenum-start)
          (setq target-line tmp-line)
          (setq max-line-length (length tmp-line)))

        (setq linenum-start (1+ linenum-start))))
    ;; build (key . linenum-start)
    (cons (format "%s %d: %s"
                  (if (eq 'deleted (aref $gutter 1)) "-" "+")
                  target-linenum target-line)
          target-linenum)))

(defun git/ivy-goto-gutter ()
  (interactive)
  (if git-gutter:diffinfos
      (let* ((collection (mapcar 'git/reshape-gutter
                                 git-gutter:diffinfos)))
        (ivy-read "git-gutters:"
                  collection
                  :action (lambda (linenum)
                            (forward-line (- (cdr linenum)
                                             (line-number-at-pos))))))
    (message "NO git-gutters!")))
;; }}

;; @see
;; http://blog.binchen.org/posts/new-git-timemachine-ui-based-on-ivy-mode.html
;; {{ git-timemachine
(defun git/ivy-timemachine-show-selected-revision ()
  "Show last (current) revision of file."
  (interactive)
  (let (collection)
    (setq collection
          (mapcar (lambda (rev)
                    ;; re-shape list for the ivy-read
                    (cons (concat (substring (nth 0 rev) 0 7) "|"
                                  (nth 5 rev) "|" (nth 6 rev)) rev))
                  (git-timemachine--revisions)))
    (ivy-read "commits:"
              collection
              :action (lambda (rev)
                        (git-timemachine-show-revision (cdr rev))))))

(defun git/ivy-timemachine ()
  "Open git snapshot with the selected version.  Based on `ivy-mode'."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (git-timemachine--start #'git/ivy-timemachine-show-selected-revision))
;; }}

(when (fboundp 'define-fringe-bitmap)
  (require 'git-gutter-fringe))
(with-eval-after-load 'git-gutter
  (when (fboundp 'define-fringe-bitmap)
    (set-face-foreground 'git-gutter-fr:modified "yellow")
    (set-face-foreground 'git-gutter-fr:added    "green")
    (set-face-foreground 'git-gutter-fr:deleted  "red")
    (setq git-gutter-fr:side 'right-fringe)
    (define-fringe-bitmap 'git-gutter-fr:modified
      [#b00000000
       #b11111111
       #b11111111
       #b00000000
       #b00000000
       #b11111111
       #b11111111
       #b00000000])
    (define-fringe-bitmap 'git-gutter-fr:deleted
      [#b00000000
       #b00000000
       #b00000000
       #b11111111
       #b11111111
       #b00000000
       #b00000000
       #b00000000])
    (define-fringe-bitmap 'git-gutter-fr:added
      [#b00011000
       #b00011000
       #b00011000
       #b11111111
       #b11111111
       #b00011000
       #b00011000
       #b00011000]))
  (setq git-gutter:update-hooks '(after-save-hook))
  (setq git-gutter:handled-backends '(svn hg git)))

;; This setup function must run before `semantic-mode' invoke to avoid
;; a error
(define-hook! git|generic-prog-mode-setup ((prog-mode-hook :append))
  (unless (or (file-remote-p default-directory)
              (buffer-temporary?)
              (> (buffer-size) core-large-buffer-size))
    (git-gutter-mode 1)))

(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t))


(define-hook! (git|message-kill-commit-id msg)
  (git-messenger:after-popup-hook)
  ;; extract commit id and put into the kill ring
  (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
    (kill-new (match-string 2 msg))
    (message "commit hash %s => kill-ring" (match-string 2 msg))))

(define-key! :prefix "C-x g"
  ("h" . git-gutter:popup-hunk)
  ("s" . git-gutter:stage-hunk)
  ("r" . git-gutter:revert-hunk)
  ("t" . git/ivy-timemachine)
  ("n" . git-gutter:next-hunk)
  ("p" . git-gutter:previous-hunk)
  ("j" . git/ivy-goto-gutter)
  ("l" . git-link)
  ("c" . git-link-commit)
  ("m" . git-messenger:popup-message)
  ("g" . magit-status))


(provide 'init-git)
