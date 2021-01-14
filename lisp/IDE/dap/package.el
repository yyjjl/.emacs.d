;; -*- lexical-binding:t -*-

(require-packages! dap-mode)

(defvar ymacs-dap-running-session-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key map (kbd "C-c C-z") #'ymacs-dap/goto-repl-buffer)
    map))
