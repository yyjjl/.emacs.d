(defvar gud--current-overlay
  (let* ((ov (make-overlay (point-min) (point-min))))
    (overlay-put ov 'face 'secondary-selection)
    ov)
  "Overlay variable for GUD highlighting.")

(defun gud*highlight-current-line ($fn $true-file $line)
  "Highlight current line."
  (let* ((ov gud--current-overlay)
         (buf (gud-find-file $true-file))
         (pos (with-current-buffer buf (point)))
         (ret (funcall $fn $true-file $line)))
    (with-current-buffer buf
      (move-overlay ov
                    (line-beginning-position)
                    (line-beginning-position 2)
                    (current-buffer))
      (goto-char pos))
    ret))
(advice-add 'gud-display-line :around #'gud*highlight-current-line)

(defun gud|kill-overlay ()
  (if (derived-mode-p 'gud-mode)
      (delete-overlay gud--current-overlay)))

;; Move the cursor to the end of last line if it's gud-mode {{
(defun gud*move-to-end (&rest $args)
  (when (eq major-mode 'gud-mode)
    (goto-char (point-max))))

(advice-add 'switch-to-buffer :after #'gud*move-to-end)
(advice-add 'pop-to-buffer :after #'gud*move-to-end)
(advice-add 'select-window-by-number :after #'gud*move-to-end)
(advice-add 'windmove-do-window-select :after #'gud*move-to-end)

(defmacro gud%gdb-display ($type)
  (let ((name (intern (format "gud/display-%s" $type))))
    `(progn
       (defun ,name (&optional no-select)
         (interactive "P")
         (popwin:popup-buffer (gdb-get-buffer-create ',$type)))
       #',name)))

(defun gud/pop-to-source-buffer ()
  (interactive)
  (when gud-last-last-frame
    (pop-to-buffer (gud-find-file (car gud-last-last-frame)))))

(defun gud/display-comint-buffer ()
  (interactive)
  (pop-to-buffer gud-comint-buffer))

(defvar gud--source-mode-map
  (define-key! :map (make-sparse-keymap)
    ("d i" . (gud%gdb-display gdb-inferior-io))
    ("d g" . gud/display-comint-buffer)
    ("d b" . (gud%gdb-display gdb-breakpoints-buffer))
    ("d t" . (gud%gdb-display gdb-threads-buffer))
    ("d r" . (gud%gdb-display gdb-registers-buffer))
    ("d s" . (gud%gdb-display gdb-stack-buffer))
    ("d d" . (gud%gdb-display gdb-disassembly-buffer))
    ("d m" . (gud%gdb-display gdb-memory-buffer))
    ("q" . gud-source-mode)
    ("=" .  gud-break)
    ("-" . gud-remove)
    ("t" . gud-tbreak)
    ("s" . gud-step)
    ("n" . gud-next)
    ("i n" . gud-nexti)
    ("i s" . gud-stepi)
    ("j" . gud-jump)
    ("f" . gud-finish)
    ("<" . gud-up)
    (">" . gud-down)
    ("c" . gud-cont)
    ("u" . gud-until)
    ("r" . gud-run)
    ("C" . gud-call)
    ("p" . gud-print)
    ("*" . gud-pstar)
    ("w" . gud-watch)
    ("x" . (lambda () (interactive)
             (when (and (yes-or-no-p "Quit process"))
               (gud-call "quit"))))))

(defvar-local gud--source-buffer-status nil)
(defvar-local gud--source-buffer-list nil)

(define-minor-mode gud-source-mode "minor mode for source file"
  :init-value nil
  :lighter "GS"
  :keymap gud--source-mode-map
  (if gud-source-mode
      (progn
        (setq gud--source-buffer-status
              (list buffer-read-only header-line-format))
        (setq header-line-format "GUD source file, press `q' to exit")
        (setq buffer-read-only t))
    (when gud--source-buffer-status
      (setq buffer-read-only (car gud--source-buffer-status))
      (setq header-line-format (cadr gud--source-buffer-status))))
  (force-mode-line-update))

(defun gud*find-file-hack ($fn $file)
  (let ((buf (funcall $fn $file)))
    (when (and buf gud-comint-buffer)
      (with-current-buffer gud-comint-buffer
        (add-to-list 'gud--source-buffer-list buf))
      (with-current-buffer buf
        (when (not gud-source-mode)
          (gud-source-mode 1)))
      buf)))
(advice-add 'gud-find-file :around #'gud*find-file-hack)

(defun gud|sentinel-hack ($proc $msg)
  (let ((buf (process-buffer $proc)))
    (when (and (buffer-live-p buf)
               (memq (process-status $proc) '(signal exit)))
      (dolist (src-buf (buffer-local-value 'buf gud--source-buffer-list))
        (when (buffer-live-p src-buf)
          (with-current-buffer src-buf
            (when gud-source-mode
              (gud-source-mode -1))))))))

(advice-add 'gud-sentinel :after #'gud|sentinel-hack)


(with-eval-after-load 'gud
  (define-hook! gud|setup-hook (gud-mode-hook)
    (add-hook 'kill-buffer-hook #'gud|kill-overlay nil :local)
    (local-set-key (kbd "C-c C-z") #'gud/pop-to-source-buffer)
    ;; `gud-print' need prompt can be modified
    ;; (setq comint-prompt-read-only t)
    ))

(with-eval-after-load 'gdb-mi
  (setq gdb-show-main t)
  (fset 'gdb-display-buffer 'display-buffer))

(provide 'init-gud)
