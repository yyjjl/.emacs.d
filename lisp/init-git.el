(bind-keys ("C-x g h" . git-gutter:popup-hunk)
           ("C-x g s" . git-gutter:stage-hunk)
           ("C-x g r" . git-gutter:revert-hunk)
           ("C-x g t" . git-gutter-mode)
           ("C-x n" . git-gutter:next-hunk)
           ("C-x p" . git-gutter:previous-hunk)
           ("C-x g l" . git-link)
           ("C-x g c" . git-link-commit)
           ("C-x g m" . git-messenger:popup-message))

(with-eval-after-load 'git-gutter
  (setq git-gutter:handled-backends '(svn hg git))
  (git-gutter:linum-setup))

(with-eval-after-load 'git-messenger
  (setq git-messenger:show-detail t)
  (add-hook 'git-messenger:after-popup-hook
            (lambda (msg)
              ;; extract commit id and put into the kill ring
              (when (string-match "\\(commit *: *\\)\\([0-9a-z]+\\)" msg)
                (copy-yank-str (match-string 2 msg))
                (message "commit hash %s => clipboard & kill-ring" (match-string 2 msg))
                ))) )

(global-git-gutter-mode 1)


(provide 'init-git)

