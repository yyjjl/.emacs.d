;;; -*- lexical-binding: t; -*-

(cl-defun ymacs-python//set-lsp-server (&optional -server)
  (when (not -server)
    (if (get 'ymacs-python-lsp-server 'initialized)
        (cl-return-from ymacs-python//set-lsp-server)
      ;; first call to this function
      (setq -server ymacs-python-lsp-server)
      (setq ymacs-python-lsp-server nil)
      (put 'ymacs-python-lsp-server 'initialized t)))

  (cl-assert (memq -server ymacs-python-lsp-servers)
             "Not in %s"
             ymacs-python-lsp-servers)

  (when (and (not (eq -server ymacs-python-lsp-server))
             (require 'lsp-mode nil t))
    (require (intern (format "lsp-%s" -server)) nil t)

    (setq ymacs-python-lsp-server -server)
    (setq lsp-disabled-clients (cl-delete -server lsp-disabled-clients))
    (dolist (server ymacs-python-lsp-servers)
      (unless (eq server -server)
        (add-to-list 'lsp-disabled-clients server)))))

(defun ymacs-python//get-or-create-process (-directory -dedicated)
  (let* ((default-directory -directory)
         (name (python-shell-get-process-name -dedicated))
         (buffer (or
                  (get-buffer (format "*%s*" name))
                  (python-shell-make-comint (python-shell-calculate-command) name t))))
    (with-current-buffer buffer
      (let ((cumtime 0))
        (while (and (when (boundp 'python-shell--first-prompt-received)
                      (not python-shell--first-prompt-received))
                    (< cumtime 3))
          (when (> cumtime 0.5)
            (message "Wait for python process ...(%s)" cumtime))
          (sleep-for 0.1)
          (setq cumtime (+ cumtime 0.1)))))
    (get-buffer-process buffer)))

(defun ymacs-python//send-region-or-buffer (&optional -send-main)
  (if (use-region-p)
      (python-shell-send-region (region-beginning) (region-end) -send-main)

    (python-shell-send-buffer -send-main)
    (when (unless -send-main
            (let ((if-main-regex "^if +__name__ +== +[\"']__main__[\"'] *:"))
              (save-excursion
                (goto-char (point-min))
                (re-search-forward if-main-regex nil t))))
      (message (concat "Removed if __name__ == '__main__' construct, "
                       "use a prefix argument to evaluate.")))))

(defun ymacs-python//enable-pdbtrack ()
  (unless (memq 'python-pdbtrack-comint-input-filter-function
                comint-output-filter-functions)
    (python-pdbtrack-setup-tracking))

  (add-to-list 'mode-line-buffer-identification
               (propertize "[pdbtrack]" 'face 'font-lock-builtin-face)
               t)

  (add-hook 'comint-output-filter-functions
            #'python-pdbtrack-comint-output-filter-function
            nil t)

  (force-mode-line-update))

(defun ymacs-python//disable-pdbtrack ()
  (remove-hook 'mode-line-buffer-identification
               "[pdbtrack]")
  (remove-hook 'comint-output-filter-functions
               #'python-pdbtrack-comint-output-filter-function
               t)
  (force-mode-line-update))

(defun ymacs-python//generate-doc (-params -indent)
  (setq -indent (concat "\n" -indent))
  (string-join (mapcar (lambda (token)
                         (let ((param (split-string token "=" t " +"))
                               default)
                           (setq default (cadr param))
                           (setq param (car param))
                           (concat "@param " param
                                   (if default
                                       (format " (default: %s): " default)
                                     ": ")
                                   -indent "@type " param ": ")))
                       (split-string -params "," t " +"))
               -indent))

(defun ymacs-python//shell-running-p ()
  (when-let* ((process (or (and (derived-mode-p 'inferior-python-mode)
                                (get-buffer-process (current-buffer)))
                           (python-shell-get-process))))
    (with-current-buffer (process-buffer process)
      (not comint-last-prompt))))
