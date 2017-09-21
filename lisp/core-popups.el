(defun core/last-popup-window ()
  "Display last popup window"
  (interactive)
  (if (buffer-live-p shackle-last-buffer)
      (display-buffer shackle-last-buffer)
    (message "Last buffer killed !!")
    (core/display-buffer)))

(defun core/display-buffer ()
  (interactive)
  (let ((ivy-use-virtual-buffers nil))
    (display-buffer
     (completing-read "Buffer:"
                      #'internal-complete-buffer
                      nil
                      :require-match))))

(defun core/fix-popup-window ()
  "Make a popup window not to close when `C-g' pressed"
  (interactive)
  (setq core--shackle-popup-window-list
        (remove (selected-window)
                core--shackle-popup-window-list)))

(defun core/popup-messages ()
  "Display *Messages* buffer"
  (interactive)
  (display-buffer (get-buffer-create "*Messages*")))

(defun core/popup-messages ()
  "Display *Messages* buffer"
  (interactive)
  (display-buffer (get-buffer-create "*Messages*")))

(defun core/popup-sdcv ()
  "Display *sdcv* buffer"
  (interactive)
  (let ((word (if (and transient-mark-mode mark-active)
                  (buffer-substring-no-properties (region-beginning)
                                                  (region-end))
                (sdcv-current-word))))
    (sdcv-goto-sdcv)
    (setq word (read-string
                (format "Word (default %s): " word)
                nil nil word))
    (sdcv-search-word word)))

(with-eval-after-load 'shackle
  (defvar shackle-mode-map
    (define-key! :map (make-sparse-keymap)
      ("l" . core/last-popup-window)
      ("d" . core/popup-sdcv)
      ("b" . core/display-buffer)
      ("RET" . core/fix-popup-window)))

  (global-set-key (kbd "C-z") shackle-mode-map)
  (global-set-key (kbd "C-x m") #'core/popup-messages)

  (defun core%shackle-align ()
    "Set default align according to frame size and window number."
    (if (> (frame-width) split-width-threshold) 'right 'below))

  (defvar core--shackle-popup-window-list nil)
  (defvar-local core--shackle-popup-window nil)
  (put 'core--shackle-popup-window 'permanent-local t)
  (define-hook! core%autoclose-popup-window (kill-buffer-hook)
    "Auto close popup window after buffer killed"
    (when core--shackle-popup-window
      (let ((win (get-buffer-window (current-buffer))))
        (when (and (equal win core--shackle-popup-window)
                   (window-live-p win)
                   (not (one-window-p))
                   (not (minibuffer-window-active-p win)))
          (delete-window win)))))

  (defun core*close-popup-window (&rest _)
    "When `C-g' pressed, close latest opened popup window"
    ;; Remove inactive window
    (setq core--shackle-popup-window-list
          (--filter (window-live-p it) core--shackle-popup-window-list))
    (let ((quit? (called-interactively-p 'interactive))
          (window (car core--shackle-popup-window-list)))
      ;; `C-g' can deactivate region
      (when (and (not (region-active-p))
                 (window-live-p window)
                 (not (one-window-p)))
        (delete-window window))))
  ;; Can not be advised `:after', keyboard-quit signal `quit'
  (advice-add 'keyboard-quit :before #'core*close-popup-window)

  (defun core*shackle-display-buffer-hack ($fn $buffer $alist $plist)
    ;; Set default size
    (let* ((root (car (window-tree)))
           (num (if (or (atom root) (car-safe root))
                    2
                  (length (cdr root)))))
      (setq shackle-default-size (min 0.4 (/ 1.0 num))))

    (let ((window (funcall $fn $buffer $alist $plist))
          (inhibit-autoclose? (plist-get $plist :inhibit-autoclose)))
      (unless inhibit-autoclose?
        ;; Add to autoclose list
        (push window core--shackle-popup-window-list))
      (with-current-buffer $buffer
        ;; Record popup window
        (setq core--shackle-popup-window window))
      window))

  (advice-add 'shackle-display-buffer
              :around #'core*shackle-display-buffer-hack)

  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-default-rule nil
        shackle-rules
        '(("*sdcv*" :align below :size 15 :select t)
          ((:custom
            (lambda (buffer)
              (or (derived-mode? 'comint-mode buffer)
                  (memq (buffer-local-value 'major-mode buffer)
                        '(term-mode inferior-ess-mode)))))
           :size 0.4 :align below :select t :inhibit-autoclose t)
          (help-mode :align below :select t)
          (ivy-occur-grep-mode :inhibit-autoclose t
                               :select t :align core%shackle-align)
          (grep-mode :inhibit-autoclose t
                     :select t :align core%shackle-align)
          (occur-mode :inhibit-autoclose t
                      :select t :align core%shackle-align)
          ("^\\*.*?\\*" :regexp t :select t :align core%shackle-align))))

(provide 'core-popups)
