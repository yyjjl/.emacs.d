(require-packages! realgud)



(defvar-local realgud--window-configuration nil)

(defun realgud*run-process-hack ($fn &rest $args)
  (let ((window-configuration (current-window-configuration))
        (command-buffer (apply $fn $args)))
    (when (buffer-live-p command-buffer)
      (with-current-buffer command-buffer
        (setq comint-scroll-to-bottom-on-output t)
        (setq-local realgud--window-configuration
                    window-configuration)))
    command-buffer))

(defun realgud*sentinel-hack ($proc $msg)
  (let* ((buffer (process-buffer $proc))
         (window-configuration (buffer-local-value
                                'realgud--window-configuration
                                buffer)))
    (when (and (buffer-live-p buffer)
               (memq (process-status $proc) '(signal exit)))
      (when (window-configuration-p window-configuration)
        (set-window-configuration window-configuration))
      (kill-buffer buffer))))

(with-eval-after-load 'realgud:pdb-init
  (setf (gethash "breakpoints" realgud:pdb-command-hash) "break"))

(with-eval-after-load 'realgud:gdb-init
  (setf (gethash "breakpoints" realgud:pdb-command-hash) "info breakpoints"))

(with-eval-after-load 'realgud
  (setq realgud-populate-common-fn-keys-function nil
        realgud-safe-mode nil)

  (defun realgud:cmd-breakpoints ($arg)
    (interactive "P")
    (realgud:cmd-run-command $arg "breakpoints"))

  (global-set-key (kbd "C-x C-a C-q") #'realgud-short-key-mode)
  (define-key realgud:shortkey-mode-map (kbd "B")
    #'realgud:cmd-breakpoints)

  (advice-add 'realgud-term-sentinel :after #'realgud*sentinel-hack)
  (advice-add 'realgud:run-process :around #'realgud*run-process-hack))


(provide 'init-realgud)
