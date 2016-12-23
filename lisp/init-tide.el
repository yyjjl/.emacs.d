(add-hook 'typescript-mode-hook
          (lambda ()
            (unless (is-buffer-file-temp)
              (tide-setup)
              (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
              ;; company is an optional dependency. You have to
              ;; install it separately via package-install
              ;; since I use global company mode ,I don't need the exp blew
              ;;(company-mode-on)
              )))

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
(add-hook 'before-save-hook 'tide-format-before-save)

;; format options
(setq tide-format-options
      '(:insertSpaceAfterFunctionKeywordForAnonymousFunctions
        t
        :placeOpenBraceOnNewLineForFunctions nil))
;; see https://github.com/Microsoft/TypeScript/blob/cc58e2d7eb144f0b2ff89e6a6685fb4deaa24fde/src/server/protocol.d.ts#L421-473 for the full list available options

;; Tide can be used along with web-mode to edit tsx files
(add-to-list 'auto-mode-alist '("\\.tsx\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.ts\\'" . typescript-mode))
(add-hook 'web-mode-hook
          (lambda ()
            (when (and (stringp buffer-file-name) (string-equal "tsx" (file-name-extension buffer-file-name)))
              (tide-setup))))

(provide 'init-tide)