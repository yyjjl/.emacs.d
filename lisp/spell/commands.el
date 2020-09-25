;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-spell/goto-previous-error ()
  "Go to previous spelling error."
  (interactive)
  (let ((pos (point))
        (min (point-min)))
    (if (and (eq (current-buffer) flyspell-old-buffer-error)
             (eq pos flyspell-old-pos-error))
        (progn
          (if (= flyspell-old-pos-error min)
              ;; goto beginning of buffer
              (progn
                (message "Restarting from end of buffer")
                (goto-char (point-max)))
            (backward-word 1))
          (setq pos (point))))
    ;; seek the next error
    (while (and (> pos min)
                (let ((ovs (overlays-at pos))
                      (r '()))
                  (while (and (not r) (consp ovs))
                    (if (flyspell-overlay-p (car ovs))
                        (setq r t)
                      (setq ovs (cdr ovs))))
                  (not r)))
      (backward-word 1)
      (setq pos (point)))
    ;; save the current location for next invocation
    (setq flyspell-old-pos-error pos)
    (setq flyspell-old-buffer-error (current-buffer))
    (goto-char pos)))

;;;###autoload
(defun ymacs-spell/dwim (&optional -whole-buffer)
  "Check spelling manually."
  (interactive "p")
  (when-let ((mode-predicate
              (or (get major-mode 'flyspell-mode-predicate)
                  (and (derived-mode-p 'prog-mode)
                       'flyspell-generic-progmode-verify))))
    (setq flyspell-generic-check-word-predicate mode-predicate))
  (cond (-whole-buffer
         (call-interactively #'flyspell-buffer))
        ((region-active-p)
         (flyspell-region (region-beginning) (region-end)))
        (t
         (flyspell-region (line-beginning-position) (line-end-position)))))

(defhydra ymacs-hydra/flyspell
  (:color blue
   :hint nil
   :body-pre (require 'flyspell nil :no-error))
  "
_SPC_ line-or-region _b_ buffer  _p_ prev _n_ next _q_ quit"
  ("b" (ymacs-spell/dwim :whole-buffer) :exit nil)
  ("SPC" (ymacs-spell/dwim) :exit nil)
  ("p" ymacs-spell/goto-previous-error :exit nil)
  ("n" flyspell-goto-next-error :exit nil)
  ("q" nil :exit t)
  ("RET" nil :exit t))
