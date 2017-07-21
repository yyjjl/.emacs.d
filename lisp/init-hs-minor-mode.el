(setq hs-minor-mode-map
      (let ((map (make-sparse-keymap)))
        (define-key map (kbd "C-x t h") 'hs-hide-block)
        (define-key map (kbd "C-x t s") 'hs-show-block)
        (define-key map (kbd "C-x t H") 'hs-hide-all)
        (define-key map (kbd "C-x t S") 'hs-show-all)
        (define-key map (kbd "C-x t t") 'hs-toggle-hiding)
        (define-key map [(shift mouse-2)] 'hs-mouse-toggle-hiding)
        map))

(defvar hs|headline-max-len 30 "*Maximum length of `hs-headline' to display.")
(defvar hs|overlay-map (make-sparse-keymap) "Keymap for hs minor mode overlay.")
(defvar hs|hide-all nil "Current state of hideshow for toggling all.")
(defvar hs|fold-all-fun nil "Function to fold all.")
(defvar hs|fold-fun nil "Function to fold.")
(make-variable-buffer-local 'hs|fold-all-fun)
(make-variable-buffer-local 'hs|fold-fun)

(defun hs|display-headline ()
  (let* ((len (length hs-headline))
         (headline hs-headline)
         (postfix ""))
    (when (>= len hs|headline-max-len)
      (setq postfix "...")
      (setq headline (substring hs-headline 0 hs|headline-max-len)))
    (if hs-headline (concat headline postfix " ") "")))

(defun hs|abstract-overlay (ov)
  (let* ((start (overlay-start ov))
         (end (overlay-end ov))
         (str (format " ...%d... " (count-lines start end))) text)
    (setq text (propertize str 'face 'font-lock-builtin-face
                           'help-echo (buffer-substring (1+ start) end)))
    (overlay-put ov 'display text)
    (overlay-put ov 'pointer 'hand)
    (overlay-put ov 'keymap hs|overlay-map)))

(defun hs|toggle-hiding-all ()
  "Toggle hideshow all."
  (interactive)
  (setq hs|hide-all (not hs|hide-all))
  (if hs|hide-all
      (hs-hide-all)
    (hs-show-all)))

(defun hs|toggle-fold-all ()
  "Toggle fold all."
  (interactive)
  (if hs|fold-all-fun
      (call-interactively hs|fold-all-fun)
    (hs|toggle-hiding-all)))

(defun hs|toggle-fold ()
  "Toggle fold."
  (interactive)
  (if hs|fold-fun
      (call-interactively hs|fold-fun)
    (hs-toggle-hiding)))

(with-eval-after-load 'hideshow
  (setq hs-isearch-open t)
  (setq hs-set-up-overlay 'hs|abstract-overlay)
  (make-local-variable 'hs|hide-all)
  (defun hs|auto-expand ()
    (save-excursion (hs-show-block)))
  (advice-add 'goto-line :after #'hs|auto-expand)
  (advice-add 'find-tag :after #'hs|auto-expand))


(provide 'init-hs-minor-mode)
