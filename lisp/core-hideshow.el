;;; -*- lexical-binding: t; -*-

(defvar hs--overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

(defun hs//abstract-overlay (-ov)
  (let* ((start (overlay-start -ov))
         (end (overlay-end -ov))
         (str (format " ...%d... " (count-lines start end))) text)
    (setq text (propertize str 'face 'font-lock-builtin-face))
    (overlay-put -ov 'display text)
    (overlay-put -ov 'pointer 'hand)
    (overlay-put -ov 'keymap hs--overlay-map)))

(defun hs//auto-expand (&rest _)
  (save-excursion (hs-show-block)))

(config! hideshow
  :bind
  (:map hs-minor-mode-map
   ("C-x t h" . hs-hide-block)
   ("C-x t s" . hs-show-block)
   ("C-x t H" . hs-hide-all)
   ("C-x t S" . hs-show-all)
   ("C-x t l" . hs-hide-level)
   ("C-x t t" . hs-toggle-hiding))

  :advice
  (:after goto-line :name hs//auto-expand)
  (:after find-tag :name hs//auto-expand)

  :config
  (setq hs-isearch-open t)
  (setq hs-allow-nesting t)
  (setq hs-set-up-overlay 'hs//abstract-overlay)

  (hs-persistent-mode 1))


(provide 'core-hideshow)
