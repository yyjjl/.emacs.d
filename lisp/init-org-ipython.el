(setvar!
 org-has-ipython-p (executable-find "ipython3"))

(require-packages!
 ;; IPython notebook feature in `org-mode'
 (ob-ipython :when org-has-ipython-p))



(defvar org--ipython-parent-buffer nil)
(defvar org--ipython-src-block nil)
(defvar-local org--ipython-error-line 0)

(defun org//ipython-trace-move ($n)
  (let ((search-func (if (> $n 0)
                         #'re-search-forward
                       (setq $n (- 0 $n))
                       #'re-search-backward))
        found)
    (while (and (> $n 0)
                (setq found
                      (apply search-func '("^-+> \\([0-9]+\\)" nil t))))
      (setq $n (1- $n)))
    found))

(defun org/ipython-trace-prev (&optional $n)
  (interactive "p")
  (unless (org//ipython-trace-move (- $n))
    (message "No previous frame")))

(defun org/ipython-trace-next (&optional $n)
  (interactive "p")
  (unless (org//ipython-trace-move $n)
    (message "No next frame")))

(defun org/ipython-jump ($lineno &optional $do-jump)
  (interactive (list (or org--ipython-error-line 0) t))
  (if (and (buffer-live-p org--ipython-parent-buffer)
           org--ipython-src-block)
      (let ((p (org-babel-where-is-src-block-head org--ipython-src-block))
            (window (if $do-jump
                        (progn (pop-to-buffer org--ipython-parent-buffer)
                               (selected-window))
                      (display-buffer org--ipython-parent-buffer))))
        (with-selected-window window
          (goto-char p)
          (forward-line $lineno)
          (recenter)
          (point)))
    (message "Parent buffer killed or Can not find src block !!!")))

(defun org/ipython-trace-bury-buffer ()
  (interactive)
  (org/ipython-jump org--ipython-error-line)
  (call-interactively 'quit-window))

(defun org*ipython-before-execute (&rest $args)
  (setq org--ipython-parent-buffer (current-buffer))
  (setq org--ipython-src-block (org-element-context)))

(defun org*ipython-trace-setup ($fn &rest $args)
  (with-current-buffer (apply $fn $args)
    (use-local-map (copy-keymap special-mode-map))
    (define-key! :map (current-local-map)
      ("q" . org/ipython-trace-bury-buffer)
      ("p" . org/ipython-trace-prev)
      ("n" . org/ipython-trace-next)
      ("j" . org/ipython-jump))

    (goto-char (point-min))
    (if (re-search-forward "-+> \\([0-9]+\\)" nil t)
        (setq org--ipython-error-line (string-to-number (match-string 1)))
      (goto-char (point-min))
      (when (re-search-forward "SyntaxError:" nil t)
        (goto-char (point-min))
        ;; Get the line number
        (when (re-search-forward "File.*, line \\([0-9]+\\)" nil t)
          (goto-char (match-end 0))
          (setq org--ipython-error-line (string-to-number (match-string 1))))))
    (goto-char (point-min))
    (setq header-line-format
          (format "Error at line: %d, press `j' to jump to location"
                  org--ipython-error-line))))

(with-eval-after-load 'ob-ipython
  (advice-add 'org-babel-execute:ipython
              :before #'org*ipython-before-execute)
  (advice-add 'ob-ipython--create-traceback-buffer
              :around #'org*ipython-trace-setup)
  ;; Don't prompt me to confirm everytime I want to evaluate a block
  (setq org-confirm-babel-evaluate nil)

  (define-hook! org|babel-after-execute
    ((org-babel-after-execute-hook :append))
    (dolist (buf (buffer-list))
      (when (and (string-prefix-p " *http " (buffer-name buf))
                 (let ((proc (get-buffer-process buf)))
                   (not (and proc (process-live-p proc)))))
        (kill-buffer buf)))
    ;; Display/update images in the buffer after I evaluate
    (org-display-inline-images t)))


(provide 'init-org-ipython)
