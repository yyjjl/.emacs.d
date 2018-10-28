;;; -*- lexical-binding: t; -*-

(setvar!
 rust-has-rls-p (executable-find "rls"))

(require-packages!
 (ggtags :when env-has-gtags-p)
 (lsp-mode :when rust-has-rls-p)
 (lsp-rust :when rust-has-rls-p)
 (company-lsp :when rust-has-rls-p)
 cargo
 ;; Emacs 26 has conf-toml-mode
 (toml-mode :when (<= emacs-major-version 25))
 rust-mode)

(defcustom rust-config-build-lib :unset
  "Enable(t)/Disable(nil) building the lib target."
  :group 'lsp-rust
  :type 'directory
  :safe (lambda (x) (or (eq x :unset) (booleanp x))))

(defcustom rust-config-build-bin :unset
  "The bin target to build."
  :group 'lsp-rust
  :type 'directory
  :safe (lambda (x) (or (eq x :unset) (stringp x))))

(defcustom rust-config-cfg-test :unset
  "Enable(t)/Disable(nil) #[cfg(test)]."
  :group 'lsp-rust
  :type 'directory
  :safe (lambda (x) (or (eq x :unset) (booleanp x))))

(defvar rust-cargo-commands
  (eval-when-compile
    (let (commands)
      (mapatoms (lambda (symbol)
                  (let ((name (symbol-name symbol)))
                    (when (and (string-prefix-p "cargo-process" name)
                               (commandp symbol)
                               (not (memq symbol '(cargo-process-mode
                                                   cargo-process-run))))
                      (push (cons (string-join (cddr (split-string name "-")) "-")
                                  symbol)
                            commands)))))
      commands)))



(define-hook! rust|setup (rust-mode-hook)
  (cargo-minor-mode 1)
  (electric-operator-mode 1)
  (unless (or (not rust-has-rls-p)
              (buffer-temporary-p)
              (file-remote-p default-directory)
              (> (buffer-size) core-large-buffer-size))
    (add-transient-hook! (hack-local-variables-hook
                          :local t :name rust|setup-interal)
      (unless (eq rust-config-build-bin :unset)
        (lsp-rust-set-build-bin rust-config-build-bin))
      (unless (eq rust-config-build-lib :unset)
        (lsp-rust-set-build-bin rust-config-build-lib))
      (unless (eq rust-config-cfg-test :unset)
        (lsp-rust-set-build-bin rust-config-cfg-test))
      (with-demoted-errors "Lsp-rust: %s"
        (lsp-rust-enable)))))

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

(with-eval-after-load 'rust-mode
  (require 'lsp-rust nil t)
  (require 'cc-mode nil t)

  (setq rust-cargo-bin "~/.cargo/bin/cargo")
  (define-key! :map rust-mode-map
    ("C-c C-f")
    ([remap delete-char] . c-hungry-delete-forward)
    ([remap delete-backward-char] . c-hungry-delete-backwards)
    ("C-c b" . rust-format-buffer)
    ("C-c C-b" . rust-format-buffer)))

(with-eval-after-load 'cargo
  (setq cargo-process--custom-path-to-bin "~/.cargo/bin/cargo"
        cargo-process--command-flags "--color never")

  (define-key! :map cargo-minor-mode-map
    ("C-c C-c" . rust/cargo-dispatch)
    ("C-c C-l" . rust/cargo-run)))

(provide 'init-rust)
