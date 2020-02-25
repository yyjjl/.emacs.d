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

;; This setup function must run before `semantic-mode' invoke to avoid a error
(define-hook! git|generic-prog-mode-setup ((prog-mode-hook :append) org-mode-hook)
  (when (buffer-enable-rich-feature-p)
    (git-gutter-mode 1)))


(config! git-gutter
  :config
  (when (fboundp 'define-fringe-bitmap)
    (require 'git-gutter-fringe))

  (when (fboundp 'define-fringe-bitmap)
    (set-face-foreground 'git-gutter-fr:modified "yellow")
    (set-face-foreground 'git-gutter-fr:added "green")
    (set-face-foreground 'git-gutter-fr:deleted "red")
    (setq git-gutter-fr:side 'right-fringe))
  (setq git-gutter:update-hooks '(after-save-hook))
  (setq git-gutter:handled-backends '(svn hg git)))

(config! git-messenger
  :hook
  ((kill-commit-id msg)
   :define (git-messenger:after-popup-hook)
   ;; extract commit id and put into the kill ring
   (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
     (kill-new (match-string 2 msg))
     (message "commit hash %s => kill-ring" (match-string 2 msg)))))

(config! magit
  :hook (magit-todos-mode (magit-status-mode-hook))
  :config
  (setq magit-completing-read-function 'ivy-completing-read)
  ;; Disable internal vc
  (setq vc-handled-backends nil)
  (setq magit-auto-revert-mode nil))

(config! transient
  :config
  (setq transient-mode-line-format nil))

(config! magit-files
  :bind
  (:map magit-file-mode-map
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
