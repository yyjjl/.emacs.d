;; -*- lexical-binding: t; -*-

(defun ymacs-pdf//find-file-other-window (-filename &optional -wildcards)
  (let ((window (selected-window))
        (buffer (current-buffer)))
    (find-file-other-window -filename -wildcards)
    ;; FIXME pdf-tools
    (with-selected-window window
      (unless (equal (current-buffer) buffer)
        (switch-to-buffer buffer :norecord)))))

(defun ymacs-pdf//open-file (-filename)
  (if (string-suffix-p ".PDF" (upcase -filename))
      (ymacs-pdf//find-file-other-window -filename)
    (find-file -filename)))

(defun ymacs-pdf/open-pdfs ()
  (interactive)
  (find-file
   (ivy-read
    "Pdf: "
    (--filter
     (and (stringp it)
          (string-suffix-p "PDF" (upcase it)))
     recentf-list)
    :require-match t)))
