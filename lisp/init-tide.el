(add-hook 'typescript-mode-hook
          (lambda ()
            (unless (is-buffer-file-temp)
              (tide-setup)
              (setq flycheck-check-syntax-automatically
                    '(save idle-change mode-enabled)))))

(with-eval-after-load 'tide
  ;; formats the buffer before saving
  (add-hook 'before-save-hook 'tide-format-before-save)

  ;; format options
  (setq tide-format-options
        '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
          t
          :placeOpenBraceOnNewLineForFunctions nil)))

;; Tide can be used along with web-mode to edit tsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (and (stringp buffer-file-name)
                      (string-equal "tsx" (file-name-extension buffer-file-name)))
              (tide-setup))))

(provide 'init-tide)