;;; -*- lexical-binding: t; -*-

(defun ymacs-default//get-other-error-buffers (-current-buffer -current-error-buffer)
  (cl-loop
   for buffer in (buffer-list)
   when
   (and (not (eq -current-error-buffer buffer))
        (or (eq 'TeX-output-mode (buffer-local-value 'major-mode buffer))
            (and (next-error-buffer-p buffer)
                 (or (eq buffer -current-buffer)
                     (with-current-buffer buffer
                       (apply 'derived-mode-p ymacs-default-next-error-buffer-modes))))))
   collect
   (cons (format "%-50s => %s" (buffer-name buffer)
                 (buffer-local-value 'next-error-function buffer))
         buffer)))

;;;###autoload
(defun ymacs-default/select-error-buffer ()
  (interactive)
  (let* ((error-buffer (next-error-find-buffer))
         (buffers (ymacs-default//get-other-error-buffers (current-buffer) error-buffer))
         (buffer
          (cdr-safe
           (if (> (length buffers) 1)
               (completing-read! (format "(current: %s): "
                                         (when error-buffer (buffer-name error-buffer)))
                                 buffers)
             (car buffers)))))
    (unless buffer
      (user-error "Nothing to do"))
    (message "Set `%s' as error buffer" buffer)
    (setq next-error-buffer buffer)
    (setq next-error-last-buffer buffer)))

;;;###autoload
(defun ymacs-default/create-scratch-buffer (&optional -major-mode)
  "Create a new scratch buffer to work in. (could be *scratch* - *scratchX*)."
  (interactive
   (list
    (if current-prefix-arg
        (intern
         (completing-read
          "Select Mode: " obarray
          (lambda (sym)
            (and (fboundp sym)
                 (string-suffix-p "-mode" (symbol-name sym))))
          :require-match))
      major-mode)))
  (let ((buffer (get-buffer-create
                 (cl-loop
                  for n from 0
                  for name = (format "*scratch%s*" (if (= n 0) "" n))
                  unless (get-buffer name)
                  return name))))
    (with-current-buffer buffer
      (funcall -major-mode))
    (pop-to-buffer buffer)))

;;;###autoload
(defun ymacs-default/cleanup-buffer-safe ()
  "Perform a bunch of safe operations on the whitespace content of a buffer.
Does not indent buffer, because it is used for a
`before-save-hook', and that might be bad."
  (interactive)
  (untabify (point-min) (point-max))
  (delete-trailing-whitespace))

;;;###autoload
(defun ymacs-default/occur-dwim ()
  (interactive)
  (let* ((candidate
          (if (region-active-p)
              (buffer-substring-no-properties (region-beginning) (region-end))
            (when-let (sym (thing-at-point 'symbol))
              (concat "\\_<" (regexp-quote sym) "\\_>"))))
         (regexp-history
          (if candidate
              (cons candidate regexp-history)
            regexp-history)))
    (call-interactively 'occur)))

;;;###autoload
(defun ymacs-default/font-faces-at-point ()
  "Get the font face under cursor."
  (interactive)
  (let* ((pos (point))
         (text (buffer-substring pos (1+ pos)))
         (faces (-uniq  (-flatten (list (get-char-property pos 'face)
                                        (get-char-property 0 'face text))))))
    (message "%s" faces)))

;;;###autoload
(defun ymacs-default/restore-files ()
  (interactive)
  (seq-do-interactively!
   #'find-file
   (lambda (file) (format "Open %s" file))
   ymacs-default-visited-files-list
   #'find-buffer-visiting))
