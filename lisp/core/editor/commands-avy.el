;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-editor/goto-char-or-minibuffer ()
  "If minibuffer-window is active and not selected, select it.
If current-prefix-arg == (16), jump to first char before (point) in current line.
If current-prefix-arg is non-nol, jump to first char after (point) in current line.
Otherwise call `avy-goto-char-in-line'
"
  (interactive)
  (let ((window (active-minibuffer-window)))
    (cond
     ((and window (not (eq window (selected-window))))
      (select-window window))
     (current-prefix-arg
      (if (equal current-prefix-arg '(16))
          (search-backward (char-to-string (read-char "backward to char:"))
                           (line-beginning-position))
        (search-forward (char-to-string (read-char "forward to char:"))
                        (line-end-position))))
     ((let ((avy-single-candidate-jump t)
            (avy-all-windows nil))
        (call-interactively #'avy-goto-char-in-line))))))

(defun ymacs-editor//avy-action-copy--prompt ()
  (format
   "%s to kill, %s/%s/%s to copy, %s/%s to expand/reset: %s"
   (propertize "k" 'face font-lock-keyword-face)
   (propertize "w" 'face font-lock-keyword-face)
   (propertize "RET" 'face font-lock-keyword-face)
   (propertize "SPC" 'face font-lock-keyword-face)
   (propertize "-" 'face font-lock-keyword-face)
   (propertize "0" 'face font-lock-keyword-face)
   (mapconcat
    (lambda (item)
      (format "%s=%s"
              (propertize (char-to-string (car item)) 'face 'font-lock-keyword-face)
              (cdr item)))
    ymacs-editor-avy-copy-key-alist
    " ")))

(defun ymacs-editor//avy-action-copy (-point &optional -do-yank)
  "Kill thing at PT."
  (save-mark-and-excursion
    (goto-char -point)
    (save-excursion
      (forward-sexp)
      (set-mark (point)))
    (let ((prompt (ymacs-editor//avy-action-copy--prompt))
          done
          char)
      (unwind-protect
          (while (and (not done)
                      (setq char (read-char prompt t)))
            (cond
             ((or (eq char ?k) (eq char ?K))
              (setq done 'kill))
             ((eq char ?-)
              (er/expand-region 1)
              t)
             ((eq char ?0)
              (er/contract-region 1)
              t)
             ((or (eq char 13) (eq char 32) (eq char ?w))
              (setq done 'copy))
             ((when-let (thing (alist-get char ymacs-editor-avy-copy-key-alist))
                (if-let (bounds (bounds-of-thing-at-point thing))
                    (save-excursion
                      (goto-char (cdr bounds))
                      (set-mark (point))
                      (goto-char (car bounds)))
                  (lv-message "No %s at point" thing))
                t))
             (t
              (lv-message "Invalid key %c" char))))
        (lv-delete-window))
      (call-interactively
       (cl-case done
         (kill #'kill-region)
         (copy #'kill-ring-save)))
      (message "%s %s" done (current-kill 0))))
  (select-window (cdr (ring-ref avy-ring 0)))
  (when -do-yank
    (yank))
  t)

;;;###autoload
(defun ymacs-editor/avy-copy ()
  (interactive)
  (let* ((avy-single-candidate-jump nil)
         (old-pre-action avy-pre-action)
         (avy-pre-action
          (lambda (&rest -args)
            (setq avy-action #'ymacs-editor//avy-action-copy)
            (apply old-pre-action -args))))
    (call-interactively #'avy-goto-word-or-subword-1)))

;;;###autoload
(defun ymacs-editor/avy-copy-and-yank ()
  (interactive)
  (let* ((avy-single-candidate-jump nil)
         (old-pre-action avy-pre-action)
         (avy-pre-action
          (lambda (&rest -args)
            (setq avy-action
                  (lambda (-point) (ymacs-editor//avy-action-copy -point :yank)))
            (apply old-pre-action -args))))
    (call-interactively #'avy-goto-word-or-subword-1)))
