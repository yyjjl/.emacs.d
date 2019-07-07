(require-packages!
 gitignore-mode
 gitconfig-mode
 git-gutter
 git-gutter-fringe
 git-timemachine
 hl-todo
 magit-todos
 magit)



(defun git//format-gutter (-gutter)
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
               (let ((string (nth 1 (split-string (aref -gutter 2)
                                                  "\n" t))))
                 (if (string-match-p "^-\\|\\+\\|=" string)
                     (substring string 1)
                   string))))
     start-line)))

(defun git/goto-gutter ()
  (interactive)
  (unless git-gutter:diffinfos
    (error "No git-gutters!"))
  (let ((gutters (mapcar #'git//format-gutter git-gutter:diffinfos)))
    (ivy-read "git-gutters:" gutters
              :require-match t
              :action (lambda (x)
                        (forward-line (- (cdr x) (line-number-at-pos))))
              :keymap (define-key! :map (make-sparse-keymap)
                        ("C-n" . ivy-next-line-and-call)
                        ("C-p" . ivy-previous-line-and-call)))))

(defun git/timemachine-show-selected-revision ()
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

(defun git/timemachine ()
  "Open git snapshot with the selected version.  Based on `ivy-mode'."
  (interactive)
  (unless (featurep 'git-timemachine)
    (require 'git-timemachine))
  (let ((name (buffer-name)))
    (condition-case nil
        (git-timemachine--start #'git/timemachine-show-selected-revision)
      (quit (kill-buffer (format "timemachine:%s" name))))))


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
              (buffer-temporary-p)
              (> (buffer-size) core-large-buffer-size))
    (git-gutter-mode 1)))

(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; Disable interal vc
  (setq vc-handled-backends nil)
  (setq magit-auto-revert-mode nil)

  (add-hook 'magit-status-mode-hook 'magit-todos-mode))

(with-eval-after-load 'transient
  (setq transient-mode-line-format nil))

(with-eval-after-load 'magit-files
  (define-key! :map magit-file-mode-map
    ("C-x g g" . magit-status)
    ("C-x g")))

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
  ("t" . git/timemachine)
  ("n" . git-gutter:next-hunk)
  ("p" . git-gutter:previous-hunk)
  ("j" . git/goto-gutter)
  ("g" . magit-status)
  ;; ("l" . git-link)
  ;; ("c" . git-link-commit)
  ;; ("m" . git-messenger:popup-message)
  ("b" . magit-checkout))


(provide 'init-git)
