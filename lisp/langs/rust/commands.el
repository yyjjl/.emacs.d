;;; -*- lexical-binding: t; -*-

;;;###autoload
(defun ymacs-rust/cargo-dispatch (-use-last-action)
  (interactive "P")
  (let* ((last-action (or (and (boundp 'ymacs-rust-cargo-history)
                               (car-safe ymacs-rust-cargo-history))
                          "build"))
         (last-command (cdr (assoc last-action ymacs-rust-cargo-commands))))
    (if (and -use-last-action last-command)
        (call-interactively last-command)
      (ivy-read "Command: " ymacs-rust-cargo-commands
                :require-match t
                :preselect last-action
                :action (lambda (command) (call-interactively (cdr command)))
                :history 'ymacs-rust-cargo-history))))
