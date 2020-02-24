;;; -*- lexical-binding: t; -*-

(require-packages! realgud)



(defvar realgud--window-configuration nil)

(defun realgud*around-run-process (-fn &rest -args)
  (setq realgud--window-configuration (current-window-configuration))
  (delete-other-windows)
  (if (>= (frame-width) (or split-width-threshold 120))
      (split-window-horizontally)
    (split-window-vertically))
  (when-let* ((buffer (apply -fn -args))
              (window (get-buffer-window)))
    (set-window-dedicated-p window t)
    buffer))

(defun realgud*after-term-sentinel (-proc _)
  (let ((buffer (process-buffer -proc)))
    (when (and (buffer-live-p buffer)
               (memq (process-status -proc) '(signal exit)))
      ;; Do not kill buffer, save breakpoints
      ;; (kill-buffer buffer)
      (when (window-configuration-p realgud--window-configuration)
        (set-window-configuration realgud--window-configuration)
        (setq realgud--window-configuration nil)))))

(with-eval-after-load 'realgud:pdb-init
  (setf (gethash "breakpoints" realgud:pdb-command-hash) "break")
  (setf (gethash "display" realgud:pdb-command-hash) "display %s")
  (setf (gethash "display-all" realgud:pdb-command-hash) "display"))

(with-eval-after-load 'realgud:gdb-init
  (setf (gethash "breakpoints" realgud:gdb-command-hash) "info breakpoints"))

(with-eval-after-load 'realgud-gdb
  (define-key! :map realgud:gdb-track-mode-map
    ("C-c C-z" . realgud/jump-to-srcbuf)
    ("`" :map realgud:shortkey-mode-map)))

(with-eval-after-load 'realgud-track-mode
  (define-key! :map realgud-track-mode-map
    ("C-c C-z" . realgud/jump-to-srcbuf)
    ("`" :map realgud:shortkey-mode-map)))

(defvar realgud--saved-breakpoints nil)
(defun realgud*save-breakpoints (-buffer)
  (when (buffer-live-p -buffer)
    (with-current-buffer -buffer
      (setq realgud--saved-breakpoints
            (cl-loop for loc in (realgud-cmdbuf-info-bp-list realgud-cmdbuf-info)
                     collect (let ((line-num (realgud-loc-line-number loc))
                                   (filename (realgud-loc-filename loc)))
                               (cons filename line-num)))))))

(defun realgud*cmd-quit (_arg)
  (let ((cmdbuf (realgud-get-cmdbuf)))
    (when cmdbuf
      (pop-to-buffer cmdbuf))))

(with-eval-after-load 'realgud
  (fset 'realgud-populate-common-fn-keys-standard 'ignore)
  (setq realgud-populate-common-fn-keys-function nil)
  (setq realgud-safe-mode nil)

  (define-key! :map realgud:shortkey-mode-map
    ("M" . realgud/cmd-display)
    ("B" . realgud/cmd-breakpoints)
    ("G" . realgud/jump-to-cmdbuf)
    ("L" . realgud/restore-breakpoints)
    ("C-c C-z" . realgud/jump-to-cmdbuf))

  (advice-add 'realgud-term-sentinel :after #'realgud*after-term-sentinel)
  (advice-add 'realgud:run-process :around #'realgud*around-run-process)
  (advice-add 'realgud:terminate :before #'realgud*save-breakpoints)
  (advice-add 'realgud:cmd-quit :before #'realgud*cmd-quit))

(with-eval-after-load 'comint
  (define-key! :map comint-mode-map
    ([F5] . realgud-track-mode)
    ("M-h" . counsel-shell-history)))

(provide 'init-realgud)
