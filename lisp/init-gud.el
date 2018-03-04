(require-packages! gud gdb-mi)

(defvar gdb--info-window nil)
(defvar gud--window-configuration nil)
(defvar-local gud--source-buffer-status nil)
(defvar-local gud--source-buffer-list nil)

(defmacro gud%gdb-display ($type)
  (let ((name (intern (format "gud/display-%s" $type))))
    `(progn
       (defun ,name (&optional $no-select)
         (interactive "P")
         (let ((old-win (selected-window))
               (buffer (gdb-get-buffer-create ',$type)))
           (unless (window-live-p gdb--info-window)
             (when-let ((comint-buffer gud-comint-buffer)
                        (win (get-buffer-window comint-buffer)))
               (with-selected-window win
                 (setq gdb--info-window
                       (split-window-vertically -10)))))
           (if (window-live-p gdb--info-window)
               (progn
                 (with-selected-window gdb--info-window
                   (set-window-dedicated-p gdb--info-window nil)
                   (switch-to-buffer buffer)
                   (set-window-dedicated-p gdb--info-window t))
                 (unless $no-select
                   (select-window gdb--info-window)))
             (message "Can not display buffer %s" '$type))))
       #',name)))

;; Fix a bug when call commands from source buffer
(defun gud*display-line-hack (&rest _)
  (when (and gud-overlay-arrow-position
             (buffer-live-p (marker-buffer gud-overlay-arrow-position)))
    (when-let* ((buffer (marker-buffer gud-overlay-arrow-position))
                (window (and (buffer-live-p buffer)
                             (get-buffer-window buffer))))
      (with-selected-window window
        (when (save-excursion
                (goto-char gud-overlay-arrow-position)
                (swiper--ensure-visible)
                (forward-line 2)
                (not (pos-visible-in-window-p)))
          (recenter))))))

(defun gud/pop-to-source-buffer ()
  (interactive)
  (when-let ((file (or (car gud-last-last-frame)
                       gdb-main-file)))
    (pop-to-buffer (gud-find-file file))))

(defun gud/pop-to-comint-buffer ()
  (interactive)
  (pop-to-buffer gud-comint-buffer))

(defvar gud--source-mode-map
  (define-key! :map (make-sparse-keymap)
    ("o" . (gud%gdb-display gdb-inferior-io))
    ("g" . gud/pop-to-comint-buffer)
    ("B" . (gud%gdb-display gdb-breakpoints-buffer))
    ("T" . (gud%gdb-display gdb-threads-buffer))
    ("R" . (gud%gdb-display gdb-registers-buffer))
    ("S" . (gud%gdb-display gdb-stack-buffer))
    ("D" . (gud%gdb-display gdb-disassembly-buffer))
    ("m" . (gud%gdb-display gdb-memory-buffer))
    ("b" . gud-break)
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
    ("X" . (lambda!
             (when (and (yes-or-no-p "Quit process"))
               (gud-call "quit"))))
    ("C-c C-z" . (lambda!
                   (when (window-live-p gdb--info-window)
                     (select-window gdb--info-window))))))

(define-minor-mode gud-source-mode "minor mode for source file"
  :init-value nil
  :lighter "GS"
  :keymap gud--source-mode-map
  (if gud-source-mode
      (progn
        (setq gud--source-buffer-status (list buffer-read-only))
        (add-to-list 'mode-line-misc-info
                     '(gud-source-mode "Debugging"))
        (setq buffer-read-only t))
    (when gud--source-buffer-status
      (remove-hook 'mode-line-misc-info
                   '(gud-source-mode "Debugging"))
      (setq buffer-read-only (car gud--source-buffer-status))))
  (force-mode-line-update))

(defun gud*find-file-hack ($fn $file)
  (let ((buffer (funcall $fn $file)))
    (when (and buffer gud-comint-buffer)
      (with-current-buffer gud-comint-buffer
        (add-to-list 'gud--source-buffer-list buffer))
      (with-current-buffer buffer
        (when (not gud-source-mode)
          (gud-source-mode 1)))
      buffer)))

(defun gud*comint-init-hack (&rest _)
  (setq gud--window-configuration (current-window-configuration)))

(defun gud*sentinel-hack ($proc $msg)
  (let ((buffer (process-buffer $proc)))
    (when (and (buffer-live-p buffer)
               (memq (process-status $proc) '(signal exit)))
      (dolist (src-buf (buffer-local-value 'gud--source-buffer-list buffer))
        (when (buffer-live-p src-buf)
          (with-current-buffer src-buf
            (when gud-source-mode
              (gud-source-mode -1)))))
      (kill-buffer buffer)
      (when (window-configuration-p gud--window-configuration)
        (set-window-configuration gud--window-configuration)
        (setq gud--window-configuration nil)))))

(with-eval-after-load 'gud
  (define-key gud-mode-map "`" gud--source-mode-map)
  (define-key gud-mode-map (kbd "C-c C-z") #'gud/pop-to-source-buffer)

  (define-hook! gud|setup-hook (gud-mode-hook)
    (set-window-dedicated-p (selected-window) t)
    ;; `gud-print' need prompt can be modified
    ;; (setq comint-prompt-read-only t)
    (setq-local comint-scroll-to-bottom-on-output t))

  (advice-add 'gud-find-file :around #'gud*find-file-hack)
  (advice-add 'gud-sentinel :after #'gud*sentinel-hack)
  (advice-add 'gud-common-init :before #'gud*comint-init-hack)
  (advice-add 'gud-display-line :after #'gud*display-line-hack))

(with-eval-after-load 'gdb-mi
  (defun gdb-display-io-buffer-action (buffer alist)
    (gud/display-gdb-inferior-io :no-select))
  (defun gdb-display-io-buffer-condition (buffer action)
    (with-current-buffer buffer
      (eq major-mode 'gdb-inferior-io-mode)))

  (add-to-list 'display-buffer-alist
               '(gdb-display-io-buffer-condition
                 gdb-display-io-buffer-action))

  (dolist (map (list gdb-inferior-io-mode-map
                     gdb-breakpoints-mode-map
                     gdb-frames-mode-map
                     gdb-memory-mode-map
                     gdb-registers-mode-map
                     gdb-disassembly-mode-map
                     gdb-locals-mode-map
                     gdb-script-mode-map
                     gdb-threads-mode-map))
    (define-key map "`" gud--source-mode-map)
    (define-key map (kbd "C-c C-z") #'gud/pop-to-source-buffer))
  (setq gdb-show-main t))

(provide 'init-gud)
