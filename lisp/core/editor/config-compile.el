;;; -*- lexical-binding: t; -*-

(defvar-local ymacs-editor-compile-command-functions nil)
(defvar ymacs-editor-environment-functions ())

(defvar ymacs-editor-next-error-buffer-modes
  '(occur-mode
    grep-mode
    xref--xref-buffer-mode
    compilation-mode))


(defsubst ymacs-editor//get-environment ()
  (cl-loop for fn in ymacs-editor-environment-functions
           nconc (funcall fn)))

(defun ymacs-editor//get-error-buffer ()
  (let ((buffers
         (cl-loop
          for window in (window-list)
          for buffer = (window-buffer window)
          if (and (next-error-buffer-p buffer)
                  (with-current-buffer buffer
                    (apply 'derived-mode-p
                           ymacs-editor-next-error-buffer-modes)))
          collect buffer)))
    (when (= (length buffers) 1)
      (car buffers))))

(defun ymacs-editor//next-error-find-buffer (&rest -args)
  (or (ymacs-editor//get-error-buffer)

      (apply #'next-error-buffer-unnavigated-current -args)

      (let ((error-buffer (buffer-local-value 'next-error-buffer (current-buffer))))
        (when (and (buffer-live-p error-buffer)
                   (apply #'next-error-buffer-p error-buffer -args))
          error-buffer))))

(defun ymacs-editor//get-other-error-buffers (-current-buffer -current-error-buffer)
  (cl-loop
   for buffer in (buffer-list)
   when
   (and (not (eq -current-error-buffer buffer))
        (or (eq 'TeX-output-mode (buffer-local-value 'major-mode buffer))
            (and (next-error-buffer-p buffer)
                 (or (eq buffer -current-buffer)
                     (with-current-buffer buffer
                       (apply 'derived-mode-p ymacs-editor-next-error-buffer-modes))))))
   collect
   (cons (format "%-50s => %s" (buffer-name buffer)
                 (buffer-local-value 'next-error-function buffer))
         buffer)))



(setq next-error-find-buffer-function #'ymacs-editor//next-error-find-buffer)

(after! compile
  ;; ANSI-escape coloring in compilation-mode
  (define-hook! ymacs-default//colorize-compilation-buffer (compilation-filter-hook)
    (when (derived-mode-p 'compilation-mode)
      (ansi-color-apply-on-region compilation-filter-start (point-max))))

  (define-advice compilation-start (:around (-fn &rest -args) set-env)
    (let* ((env (ymacs-editor//get-environment))
           (buffer (with-temp-env! env
                     (apply -fn -args))))
      (with-current-buffer buffer
        (when (or env compilation-environment)
          (save-excursion
            (goto-char (point-min))
            (when (let ((case-fold-search nil))
                    (search-forward mode-name nil t))
              (forward-line 1)
              (let ((inhibit-read-only t))
                (insert "\nEnvironments:\n  ")
                (insert (string-join (append env compilation-environment) "\n  "))
                (insert "\n"))))))
      buffer))

  (define-key! :map compilation-minor-mode-map
    ("n" . ymacs-editor/grep-next-error-no-select)
    ("p" . ymacs-editor/grep-previous-error-no-select)
    ("C-o" . ymacs-editor/grep-display-error))

  (setq-default compilation-environment '("TERM=xterm-256color"))
  ;; kill compilation process before starting another
  (setq compilation-always-kill t)
  (setq compilation-scroll-output t))
