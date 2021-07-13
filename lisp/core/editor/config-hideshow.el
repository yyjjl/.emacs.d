;;; -*- lexical-binding: t; -*-

(defface ymacs-editor-hs-overlay-face
  '((t (:inherit font-lock-builtin-face)))
  "Face used for the dirname part of the buffer path."
  :group 'hideshow)

(defvar ymacs-editor-hs-overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

(defun ymacs-editor//hs-setup-overlay (-ov)
  (let ((start (overlay-start -ov))
        (end (overlay-end -ov))
        (prefix "")
        (suffix ""))
    (with-current-buffer (overlay-buffer -ov)
      (save-excursion
        (goto-char end)
        (skip-chars-backward " \t\n")
        (setq suffix (buffer-substring (point) end))
        (setq end (point)))
      (save-excursion
        (goto-char start)
        (skip-chars-forward " \t\n")
        (setq prefix (buffer-substring start (point)))
        (setq start (min end (point)))))
    ;; (move-overlay -ov start end)
    (overlay-put -ov 'display (format "%s...%d...%s" prefix  (count-lines start end) suffix))
    (overlay-put -ov 'face 'ymacs-editor-hs-overlay-face)
    (overlay-put -ov 'pointer 'hand)
    (overlay-put -ov 'keymap ymacs-editor-hs-overlay-map)))

(defun ymacs-editor//hs-auto-expand (&rest _)
  (save-excursion (hs-show-block)))

(after! hideshow
  (advice-add #'goto-line :after #'ymacs-editor//hs-auto-expand)
  (advice-add #'xref-find-definitions :after #'ymacs-editor//hs-auto-expand)

  (define-key! :map hs-minor-mode-map
    ("C-x t h" . hs-hide-block)
    ("C-x t s" . hs-show-block)
    ("C-x t H" . hs-hide-all)
    ("C-x t S" . hs-show-all)
    ("C-x t l" . hs-hide-level)
    ("C-x t t" . hs-toggle-hiding))

  (define-key! :map ymacs-editor-hs-overlay-map
    ("RET" . hs-show-block))

  (setq hs-isearch-open t)
  (setq hs-allow-nesting t)
  (setq hs-set-up-overlay #'ymacs-editor//hs-setup-overlay))
