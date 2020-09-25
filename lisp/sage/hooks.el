;;; -*- lexical-binding: t; -*-

(after! sage-shell-mode
  (define-key! :map sage-shell-mode-map
    ("C-c M-o" . ymacs-sage/clean-current-buffer)
    ("C-c C-z" . ymacs-sage/pop-to-source-buffer))

  (define-advice sage-shell-view-process-overlay (:after (-ov) set-map)
    (unless (overlay-get -ov 'extra-map)
      (let ((map (overlay-get -ov 'keymap)))
        (define-key! :map map
          ("?" . (lambda! (display-message-or-buffer
                           (format ymacs-sage-overlay-help-template
                                   (overlay-get -ov 'text)
                                   (overlay-get -ov 'math)))))
          ;; Regenerate
          ("R" . (lambda! (sage-shell-view-regenerate -ov)))
          ;; Show text
          ("T" . (lambda! (overlay-put -ov 'display nil)))
          ("w" . (lambda!
                   (sage-shell-view-copy-text -ov)
                   (message "Text copied")))
          ("W" . (lambda!
                   (sage-shell-view-copy-latex -ov)
                   (message "LaTeX copied")))
          ("=" . (lambda! (sage-shell-view--when-overlay-active
                              -ov (sage-shell-view-zoom-in -ov))))
          ("-" . (lambda! (sage-shell-view--when-overlay-active
                              -ov (sage-shell-view-zoom-out -ov)))))
        (overlay-put -ov 'extra-map t))))

  (define-hook! ymacs-sage|setup (sage-shell-after-prompt-hook)
    (when (display-graphic-p)
      (sage-shell-view-mode 1))))
