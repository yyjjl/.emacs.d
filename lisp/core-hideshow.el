(setq hs-minor-mode-map
      (define-key! :map (make-sparse-keymap)
        ("C-x t h" . hs-hide-block)
        ("C-x t s" . hs-show-block)
        ("C-x t H" . hs-hide-all)
        ("C-x t S" . hs-show-all)
        ("C-t" . hs-toggle-hiding)))

(defvar hs--headline-max-len 30
  "*Maximum length of `hs-headline' to display.")
(defvar hs--overlay-map (make-sparse-keymap)
  "Keymap for hs minor mode overlay.")

(defun hs%display-headline ()
  (let* ((len (length hs-headline))
         (headline hs-headline)
         (postfix ""))
    (when (>= len hs--headline-max-len)
      (setq postfix "...")
      (setq headline (substring hs-headline 0 hs--headline-max-len)))
    (if hs-headline (concat headline postfix " ") "")))

(defun hs%abstract-overlay ($ov)
  (let* ((start (overlay-start $ov))
         (end (overlay-end $ov))
         (str (format " ...%d... " (count-lines start end))) text)
    (setq text (propertize str 'face 'font-lock-builtin-face))
    (overlay-put $ov 'display text)
    (overlay-put $ov 'pointer 'hand)
    (overlay-put $ov 'keymap hs--overlay-map)))

(with-eval-after-load 'hideshow
  (setq hs-isearch-open t)
  (setq hs-set-up-overlay 'hs%abstract-overlay)
  (defun hs%auto-expand (&rest $args)
    (save-excursion (hs-show-block)))
  (advice-add 'goto-line :after #'hs%auto-expand)
  (advice-add 'find-tag :after #'hs%auto-expand))


(provide 'core-hideshow)
