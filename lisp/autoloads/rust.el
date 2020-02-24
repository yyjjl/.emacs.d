;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun rust/cargo-dispatch (-use-last-action)
  (interactive "P")
  (let* ((last-action (or (and (boundp 'rust-cargo-history)
                               (car-safe rust-cargo-history))
                          "build"))
         (last-command (cdr (assoc last-action rust-cargo-commands))))
    (if (and -use-last-action last-command)
        (call-interactively last-command)
      (ivy-read "Command: " rust-cargo-commands
                :require-match t
                :preselect last-action
                :action (lambda (command)
                          (call-interactively (cdr command)))
                :history 'rust-cargo-history))))
;;;###autoload
(defun rust/cargo-run ()
  (interactive)
  (let* ((project-root (or (cargo-process--project-root)
                           default-directory))
         (buffer-name (format "*Cargo: %s*" project-root))
         (buffer (get-buffer-create buffer-name))
         (proc (get-buffer-process buffer)))
    (unless (and proc
                 (process-live-p proc)
                 (eq (buffer-local-value 'major-mode buffer)
                     'term-mode))
      (kill-buffer buffer)
      (set-rust-backtrace "cargo run")
      (setq buffer (term//exec-program "cargo" '("run") buffer-name)))
    (term//pop-to-buffer buffer)))
