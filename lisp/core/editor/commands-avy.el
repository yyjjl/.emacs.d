;;; -*- lexical-binding: t; -*-

(defun ymacs-editor//avy-action-copy--prompt ()
  (format
   "%s to kill, %s/%s/%s to copy: %s"
   (propertize "k" 'face 'font-lock-major-mode)
   (propertize "w" 'face 'font-lock-major-mode)
   (propertize "RET" 'face 'font-lock-major-mode)
   (propertize "SPC" 'face 'font-lock-major-mode)
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
