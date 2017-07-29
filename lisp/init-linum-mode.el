(with-eval-after-load 'linum
  (setq main|linum-disabled-modes
        '(eshell-mode
          shell-mode
          dictionary-mode
          erc-mode
          org-mode
          browse-kill-ring-mode
          etags-select-mode
          dired-mode
          help-mode
          text-mode
          fundamental-mode
          jabber-roster-mode
          jabber-chat-mode
          inferior-js-mode
          inferior-python-mode
          inferior-scheme-mode
          twittering-mode
          compilation-mode
          weibo-timeline-mode
          woman-mode
          Info-mode
          calc-mode
          calc-trail-mode
          comint-mode
          gnus-group-mode
          inf-ruby-mode
          gud-mode
          vc-git-log-edit-mode
          log-edit-mode
          term-mode
          w3m-mode
          eww-mode
          speedbar-mode
          gnus-summary-mode
          gnus-article-mode
          calendar-mode))
  (defun main|disable-linum-mode (fn &rest args)
    (unless (memq major-mode main|linum-disabled-modes)
      (apply fn args)))
  (advice-add 'linum-mode :around #'main|disable-linum-mode))

(provide 'init-linum-mode)
