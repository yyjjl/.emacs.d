;;; -*- lexical-binding: t; -*-

(defun ymacs-hideshow//abstract-overlay (-ov)
  (let* ((start (overlay-start -ov))
         (end (overlay-end -ov))
         (str (format " ...%d... " (count-lines start end))) text)
    (setq text (propertize str 'face 'font-lock-builtin-face))
    (overlay-put -ov 'display text)
    (overlay-put -ov 'pointer 'hand)
    (overlay-put -ov 'keymap ymacs-hideshow-overlay-map)))

(defun ymacs-hideshow//auto-expand (&rest _)
  (save-excursion (hs-show-block)))
