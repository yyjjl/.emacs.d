(require-packages!
 gitignore-mode
 gitconfig-mode
 git-gutter
 git-gutter-fringe
 git-timemachine
 git-messenger
 git-link
 hl-todo
 magit-todos
 magit)


(when (fboundp 'define-fringe-bitmap)
  (require 'git-gutter-fringe))
(with-eval-after-load 'git-gutter
  (when (fboundp 'define-fringe-bitmap)
    (set-face-foreground 'git-gutter-fr:modified "yellow")
    (set-face-foreground 'git-gutter-fr:added "green")
    (set-face-foreground 'git-gutter-fr:deleted "red")
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

;; This setup function must run before `semantic-mode' invoke to avoid a error
(define-hook! git|generic-prog-mode-setup ((prog-mode-hook :append) org-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (git-gutter-mode 1)))

(define-hook! (git|message-kill-commit-id msg)
  (git-messenger:after-popup-hook)
  ;; extract commit id and put into the kill ring
  (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
    (kill-new (match-string 2 msg))
    (message "commit hash %s => kill-ring" (match-string 2 msg))))

(with-eval-after-load 'magit
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; Disable internal vc
  (setq vc-handled-backends nil)
  (setq magit-auto-revert-mode nil)

  (add-hook 'magit-status-mode-hook 'magit-todos-mode))

(with-eval-after-load 'transient
  (setq transient-mode-line-format nil))

(with-eval-after-load 'magit-files
  (define-key! :map magit-file-mode-map
    ("C-x g g" . magit-status)
    ("C-x g")))

(define-key! :prefix "C-x g"
  ("h" . git-gutter:popup-hunk)
  ("s" . git-gutter:stage-hunk)
  ("r" . git-gutter:revert-hunk)
  ("t" . git/timemachine)
  ("n" . git-gutter:next-hunk)
  ("p" . git-gutter:previous-hunk)
  ("j" . git/goto-gutter)
  ("g" . magit-status)
  ("l" . git-link)
  ("c" . git-link-commit)
  ("m" . git-messenger:popup-message)
  ("b" . magit-checkout))

(provide 'init-git)
