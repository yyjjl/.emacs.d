;;; -*- lexical-binding: t; -*-

(require-packages! realgud)

(defvar realgud--window-configuration nil)
(defvar realgud--saved-breakpoints nil)

(config! realgud:pdb-init
  :config
  (setf (gethash "breakpoints" realgud:pdb-command-hash) "break")
  (setf (gethash "display" realgud:pdb-command-hash) "display %s")
  (setf (gethash "display-all" realgud:pdb-command-hash) "display"))

(config! realgud:gdb-init
  :config
  (setf (gethash "breakpoints" realgud:gdb-command-hash) "info breakpoints"))

(config! realgud-gdb
  :bind
  (:map realgud:gdb-track-mode-map
   ("C-c C-z" . realgud/jump-to-srcbuf)
   ("`" :map realgud:shortkey-mode-map)))

(config! realgud-track-mode
  :bind
  (:map realgud-track-mode-map
   ("C-c C-z" . realgud/jump-to-srcbuf)
   ("`" :map realgud:shortkey-mode-map)))

(config! realgud
  :bind
  (:map realgud:shortkey-mode-map
   ("M" . realgud/cmd-display)
   ("B" . realgud/cmd-breakpoints)
   ("G" . realgud/jump-to-cmdbuf)
   ("L" . realgud/restore-breakpoints)
   ("C-c C-z" . realgud/jump-to-cmdbuf))

  :advice
  (:override realgud-populate-common-fn-keys-standard :name ignore)

  (:after realgud-term-sentinel
   :define (-proc _)
   (let ((buffer (process-buffer -proc)))
     (when (and (buffer-live-p buffer)
                (memq (process-status -proc) '(exit signal)))
       ;; Do not kill buffer, save breakpoints
       (when (window-configuration-p realgud--window-configuration)
         (set-window-configuration realgud--window-configuration)
         (setq realgud--window-configuration nil)))))

  (:around realgud:run-process
   :define (-fn &rest -args)
   (setq realgud--window-configuration (current-window-configuration))
   (delete-other-windows)
   (if (>= (frame-width) (or split-width-threshold 120))
       (split-window-horizontally)
     (split-window-vertically))
   (when-let* ((buffer (apply -fn -args))
               (window (get-buffer-window)))
     (set-window-dedicated-p window t)
     buffer))

  (:before realgud:terminate
   :define (-buffer)
   (when (buffer-live-p -buffer)
     (with-current-buffer -buffer
       (setq realgud--saved-breakpoints
             (--map
              (cons (realgud-loc-filename it)
                    (realgud-loc-line-number it))
              (realgud-cmdbuf-info-bp-list realgud-cmdbuf-info))))))

  (:before realgud:cmd-quit
   :define (_arg)
   (when-let (cmdbuf (realgud-get-cmdbuf))
     (pop-to-buffer cmdbuf)))

  :config
  (setq realgud-populate-common-fn-keys-function nil)
  (setq realgud-safe-mode nil))

(config! comint
  :bind
  (:map comint-mode-map ([F5] . realgud-track-mode)))

(provide 'init-realgud)
