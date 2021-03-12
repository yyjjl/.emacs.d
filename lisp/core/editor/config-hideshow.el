;;; -*- lexical-binding: t; -*-

(defface ymacs-editor-hs-overlay-face
  '((t (:inherit font-lock-builtin-face :underline t)))
  "Face used for the dirname part of the buffer path."
  :group 'hideshow)

(defvar ymacs-editor-hs-overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

(defun ymacs-editor//hs-setup-overlay (-ov)
  (let* ((start (overlay-start -ov))
         (end (overlay-end -ov))
         (str (format " ...%d... " (count-lines start end))))
    (overlay-put -ov 'display str)
    (overlay-put -ov 'face 'ymacs-editor-hs-overlay-face)
    (overlay-put -ov 'pointer 'hand)
    (overlay-put -ov 'keymap ymacs-editor-hs-overlay-map)))

(defun ymacs-editor//hs-auto-expand (&rest _)
  (save-excursion (hs-show-block)))

(after! hideshow
  (advice-add #'goto-line :after #'ymacs-editor//hs-auto-expand)
  (advice-add #'xref-find-definitions :after #'ymacs-editor//hs-auto-expand)

  (define-key! :map hs-minor-mode-map
    ("C-x t h" (defun ymacs-editor/hs-hide-block ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-block))))
    ("C-x t s" (defun ymacs-editor/hs-show-block ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-show-block))))
    ("C-x t H" (defun ymacs-editor/hs-hide-all ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-all))))
    ("C-x t S" (defun ymacs-editor/hs-show-all ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-show-all))))
    ("C-x t l" (defun ymacs-editor/hs-hide-level ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-hide-level))))
    ("C-x t t" (defun ymacs-editor/hs-toggle-hiding ()
                 (interactive)
                 (save-excursion (call-interactively #'hs-toggle-hiding)))))

  (define-key! :map ymacs-editor-hs-overlay-map
    ("RET" . hs-show-block))

  (setq hs-isearch-open t)
  (setq hs-allow-nesting t)
  (setq hs-set-up-overlay #'ymacs-editor//hs-setup-overlay))
