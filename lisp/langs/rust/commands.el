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
      (call-interactively
       (cdr (assoc-string
             (completing-read "Command: " ymacs-rust-cargo-commands
                              nil :require-match nil 'ymacs-rust-cargo-history)
             ymacs-rust-cargo-commands))))))
