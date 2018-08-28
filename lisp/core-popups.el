;;  -*- lexical-binding: t -*-
(defvar core-popups-default-regexp
  (rx line-start
      (? " ") "*" (*\? not-newline) "*" (? "<" (+ digit) ">")
      line-end))

(defvar core-popups-other-window-regexp
  (rx line-start
      "*" (or "Man" "TeX" "Shell Command Output") (*\? not-newline) "*"
      line-end))

(defvar core-popups-help-buffer-regexp
  (rx line-start
      "*"
      (or "Compile-Log"
          "sdcv"
          "lispy-message"
          (and (*\? not-newline) (in "Hh") "elp" (*\? not-newline))
          (and (*\? not-newline) (in "Dd") "escribe" (*\? not-newline)))
      "*"
      line-end))

(defvar core-popups-comint-buffer-regexp
  (rx line-start
      "*"
      (or "shell"
          "prolog"
          "Sage"
          (and (*\? not-newline) (in "Rr") "epl" (*\? not-newline))
          (and (*\? not-newline) (in "Ee") "rror" (*\? not-newline)))
      "*"
      (?  "<" (+ digit) ">")
      line-end))

(defvar core-popups--window-list nil)
(defvar-local core-popups-current-window nil)
(put 'core-popups-current-window 'permanent-local t)
(defvar core-popups-comint-modes
  '(term-mode
    haskell-interactive-mode))
(defvar core-popups-help-modes
  '(help-mode
    messages-buffer-mode
    completion-list-mode
    compilation-mode
    flycheck-error-list-mode
    profiler-report-mode))



(defsubst core-popups//clean-window-list ()
  ;; Remove inactive window
  (setq core-popups--window-list
        (--filter (window-live-p (car it)) core-popups--window-list)))

(defun core-popups*keyboard-quit-hack (&rest _)
  "When `C-g' pressed, close latest opened popup window"
  (core-popups//clean-window-list)
  ;; `C-g' can deactivate region
  (when (and (called-interactively-p 'interactive)
             (not (region-active-p)))
    (let (window action)
      (if (one-window-p)
          (progn
            (setq window (selected-window))
            (when (equal (buffer-local-value 'core-popups-current-window
                                             (window-buffer window))
                         window)
              (winner-undo)))
        (setq window (car (car core-popups--window-list)))
        (setq action (cdr (car core-popups--window-list)))
        (when (window-live-p window)
          (if (eq action 'quit)
              (quit-window nil window)
            (delete-window window))

          (pop core-popups--window-list))))))

(defun core-popups//push-window (-window -buffer &optional -autoclose-p -action)
  (when -autoclose-p
    ;; Autoclose window should be dedicated
    (set-window-dedicated-p -window t)
    ;; Add to autoclose list
    (push (cons -window (or -action 'delete)) core-popups--window-list))
  (with-current-buffer -buffer
    ;; Record popup window
    (setq core-popups-current-window -window)))

(defun core-popups*shackle-display-buffer-hack (-fn -buffer -alist -plist)
  (core-popups//clean-window-list)
  ;; Set default alignment
  (setq shackle-default-alignment
        (if (> (frame-width)
               (or split-width-threshold 120))
            'right
          'below))
  (let* ((autoclose-p (plist-get -plist :autoclose))
         (dedicated-p (plist-get -plist :dedicated))
         (old-window (get-buffer-window -buffer))
         (action 'delete)
         (window (or (funcall -fn -buffer -alist -plist)
                     (progn (setq action 'quit)
                            (funcall -fn -buffer -alist (list* :other t -plist))))))
    (unless (eq window old-window)
      (when dedicated-p
        (set-window-dedicated-p window t))
      (core-popups//push-window window -buffer autoclose-p action))
    window))

(defun core-popups//display-buffer-popup-window (-buffer -alist -plist)
  (let ((frame (shackle--splittable-frame))
        (other-window-p (and (plist-get -plist :other) (not (one-window-p)))))
    (when frame
      (let ((window (if other-window-p
                        (next-window nil 'nominibuf)
                      (shackle--split-some-window frame -alist))))
        (prog1 (window--display-buffer -buffer
                                       window
                                       (if other-window-p 'reuse 'window)
                                       -alist
                                       display-buffer-mark-dedicated)
          (when window
            (setq shackle-last-window window
                  shackle-last-buffer -buffer))
          (unless (cdr (assq 'inhibit-switch-frame -alist))
            (window--maybe-raise-frame (window-frame window))))))))

(defun core-popups*window--try-to-split-window-hack (-fn -window &optional -alist)
  (let ((buffer (and (window-live-p -window)
                     (window-buffer -window))))
    (unless (and buffer
                 (window-live-p
                  (buffer-local-value 'core-popups-current-window buffer)))
      (funcall -fn -window -alist))))

(defun core-popups//comint-buffer-matcher (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (or (memq major-mode core-popups-comint-modes)
          (derived-mode-p 'comint-mode)
          (string-match-p core-popups-comint-buffer-regexp (buffer-name))))))

(defun core-popups//help-buffer-matcher (-buffer)
  (let ((case-fold-search t))
    (with-current-buffer -buffer
      (or (and (not (string-prefix-p "ivy-occur" (symbol-name major-mode)))
               (apply #'derived-mode-p core-popups-help-modes))
          (string-match-p core-popups-help-buffer-regexp (buffer-name))))))

(with-eval-after-load 'shackle
  (defvar shackle-mode-map
    (define-key! :map (make-sparse-keymap)
      ("l" . core-popups/last-popup-window)
      ("d" . core-popups/popup-sdcv)
      ("b" . core-popups/display-buffer)
      ("RET" . core-popups/fix-popup-window)))

  (global-set-key (kbd "C-z") shackle-mode-map)
  (global-set-key (kbd "C-x m") #'view-echo-area-messages)

  ;; Can not be advised `:after', keyboard-quit signal `quit'
  (advice-add 'keyboard-quit :before #'core-popups*keyboard-quit-hack)
  (advice-add 'other-window :before #'core-popups*keyboard-quit-hack)
  (advice-add 'shackle-display-buffer
              :around #'core-popups*shackle-display-buffer-hack)
  (advice-add 'shackle--display-buffer-popup-window
              :override #'core-popups//display-buffer-popup-window)
  (advice-add 'window--try-to-split-window
              :around #'core-popups*window--try-to-split-window-hack)

  (define-hook! core-popups|autoclose-popup-window (kill-buffer-hook)
    "Auto quit popup window after buffer killed"
    (let ((window (get-buffer-window)))
      (when (and window
                 (equal window core-popups-current-window)
                 (not (one-window-p))
                 (not (minibuffer-window-active-p window))
                 (or (not (boundp 'lv-wnd))
                     (not (eq (next-window) lv-wnd))))
        (quit-window nil window))))

  (setq shackle-default-alignment 'below
        shackle-default-size 0.4
        shackle-default-rule nil
        shackle-rules
        `(((:custom core-popups//comint-buffer-matcher)
           :align below :select t :dedicated t)
          ((:custom core-popups//help-buffer-matcher)
           :align below :select t :autoclose t)
          (,core-popups-other-window-regexp :regexp t :select t :autoclose t)
          (occur-mode :select t)
          ("*Warnings*" :align below :autoclose t)
          (,core-popups-default-regexp :regexp t))))

(provide 'core-popups)
