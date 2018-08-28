;;; -*- lexical-binding: t; -*-

(setvar!
 org-has-ipython-p (executable-find "ipython3")
 org-has-sage-p (executable-find "sage"))

(require-packages!
 ;; IPython notebook feature in `org-mode'
 (ob-ipython :when org-has-ipython-p)
 (sage-shell-mode :when org-has-sage-p
                  :compile (sage-shell-mode sage-shell-view)))



(defconst org--sage-overlay-help-template
  "--------------------
Text:
%s
LaTeX:
%s
--------------------
R\tRegenerate
T\tShow text
w\tCopy text
W\tCopy LaTeX
=\tZoom in
-\tZoom out")

(defvar org--ipython-parent-buffer nil)
(defvar org--ipython-src-block nil)
(defvar-local org--ipython-error-line 0)

(defun org//ipython-trace-move (-n)
  (let ((search-func (if (> -n 0)
                         #'re-search-forward
                       (setq -n (- 0 -n))
                       #'re-search-backward))
        found)
    (while (and (> -n 0)
                (setq found
                      (apply search-func '("^-+> \\([0-9]+\\)" nil t))))
      (setq -n (1- -n)))
    found))

(defun org/ipython-trace-prev (&optional -n)
  (interactive "p")
  (unless (org//ipython-trace-move (- -n))
    (message "No previous frame")))

(defun org/ipython-trace-next (&optional -n)
  (interactive "p")
  (unless (org//ipython-trace-move -n)
    (message "No next frame")))

(defun org/ipython-jump (-lineno &optional -do-jump)
  (interactive (list (or org--ipython-error-line 0) t))
  (if (and (buffer-live-p org--ipython-parent-buffer)
           org--ipython-src-block)
      (let ((p (org-babel-where-is-src-block-head org--ipython-src-block))
            (window (if -do-jump
                        (progn (pop-to-buffer org--ipython-parent-buffer)
                               (selected-window))
                      (display-buffer org--ipython-parent-buffer))))
        (with-selected-window window
          (goto-char p)
          (forward-line -lineno)
          (recenter)
          (point)))
    (message "Parent buffer killed or Can not find src block !!!")))

(defun org/ipython-trace-bury-buffer ()
  (interactive)
  (org/ipython-jump org--ipython-error-line)
  (call-interactively 'quit-window))

(defun org*ipython-before-execute (&rest _)
  (setq org--ipython-parent-buffer (current-buffer))
  (setq org--ipython-src-block (org-element-context)))

(defun org*ipython-trace-setup (-fn &rest -args)
  (with-current-buffer (apply -fn -args)
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
    (core/delete-http-buffers :force)
    ;; Display/update images in the buffer after I evaluate
    (org-display-inline-images t)))

(defvar sage-shell:source-buffer nil)
(with-eval-after-load 'sage-shell-mode
  (fset 'sage-shell:-process-sentinel-generator 'identity)
  (advice-add 'sage-shell-view-process-overlay
              :after 'sage*after-ceare-overlay-hack)

  (define-key! :map sage-shell-mode-map
    ("C-c M-o" . sage/clea-current-buffer)
    ("C-c C-z" . sage/pop-to-source-buffer))

  (defun sage/clea-current-buffer ()
    (interactive)
    (sage-shell:-delete-output (point-min))
    (sage-shell-edit:delete-temp-dir))

  (defun sage/pop-to-source-buffer ()
    (interactive)
    (unless sage-shell:source-buffer
      (setq sage-shell:source-buffer
            (completing-read "Buffer:"
                             (mapcar
                              #'buffer-name
                              (--filter (eq 'sage-shell:sage-mode
                                            (buffer-local-value 'major-mode it))
                                        (buffer-list)))
                             nil :require-match)))
    (when sage-shell:source-buffer
      (pop-to-buffer sage-shell:source-buffer)))

  (defun sage*after-ceare-overlay-hack (-ov)
    (unless (overlay-get -ov 'extra-map)
      (let ((map (overlay-get -ov 'keymap)))
        (define-key! :map map
          ("?" . (lambda! (display-message-or-buffer
                           (format org--sage-overlay-help-template
                                   (overlay-get -ov 'text)
                                   (overlay-get -ov 'math)))))
          ;; Regenerate
          ("R" . (lambda! (sage-shell-view-regenerate -ov)))
          ;; Show text
          ("T" . (lambda! (overlay-put -ov 'display nil)))
          ("w" . (lambda!
                   (sage-shell-view-copy-text -ov)
                   (message "Text copied")))
          ("W" . (lambda!
                   (sage-shell-view-copy-latex -ov)
                   (message "LaTeX copied")))
          ("=" . (lambda! (sage-shell-view--when-overlay-active
                              -ov (sage-shell-view-zoom-in -ov))))
          ("-" . (lambda! (sage-shell-view--when-overlay-active
                              -ov (sage-shell-view-zoom-out -ov)))))
        (overlay-put -ov 'extra-map t))))

  (defun sage-shell-edit:delete-temp-dir ()
    (when (and (stringp sage-shell-edit:temp-directory)
               (string= (file-name-as-directory temporary-file-directory)
                        (file-name-directory sage-shell-edit:temp-directory))
               (file-exists-p sage-shell-edit:temp-directory))
      (message "Delete directory %s" sage-shell-edit:temp-directory)
      (delete-directory sage-shell-edit:temp-directory t)))

  (setq sage-shell-view-scale 1.5)

  (define-hook! sage|setup (sage-shell-after-prompt-hook)
    (when (display-graphic-p)
      (sage-shell-view-mode 1))
    (remove-hook 'sage-shell:process-exit-hook
                 'sage-shell-edit:delete-temp-dir)
    (add-hook 'term-or-comint-process-exit-hook
              (lambda () (run-hooks 'sage-shell:process-exit-hook))
              nil :local)
    (add-hook 'term-or-comint-process-exit-hook
              'sage-shell-edit:delete-temp-dir
              nil :local)))

(provide 'init-org-ipython)
