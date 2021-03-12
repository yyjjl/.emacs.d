;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-suround-origin-pos nil)
(defvar ymacs-editor-surround-pair-alist
  '(("()" . ("(" . ")"))
    ("{}" . ("{" . "}"))
    ("[]" . ("[" . "]"))
    ("`" . (lambda (_)
             (cons "`"
                   (if (memq major-mode '(emacs-lisp-mode lisp-interaction-mode))
                       "'"
                     "`"))))
    ("<>,." . (lambda (_)
                (let ((tag (read-string "Tag: ")))
                  (cons (concat "<" tag ">")
                        (concat "</" tag ">")))))
    ("\\" . (lambda (_)
              (let ((pair (ymacs-editor//surround-get-pair (read-char))))
                (when pair
                  (cons (concat "\\" (car pair))
                        (concat "\\" (cdr pair)))))))
    ("b" . (lambda (_)
             (let ((env (read-string "environment: ")))
               (when env
                 (cons (concat "\\begin{" env "}")
                       (concat "\\end{" env "}"))))))))

(defun ymacs-editor//surround-get-pair (-char)
  "Get pair from -CHAR"
  (let ((pair (cl-loop for (keys . pair) in ymacs-editor-surround-pair-alist
                       when (member -char (string-to-list keys))
                       return pair)))
    (cond ((functionp pair) (funcall pair -char)) ; function
          (pair pair)                             ; normal
          ((eq -char 13) nil)
          (t (cons (char-to-string -char)
                   (char-to-string -char))))))

(defun ymacs-editor//surround-get-bounds (-left -right)
  "Get bounds of pair. If -LEFT and -RIGHT is a open/close delimeter.
Use `scan-lists', otherwise use simple algorithm."
  (if (and (string-match-p "^\\s($" -left)
           (string-match-p "^\\s)$" -right))
      (ignore-errors
        (cons
         (scan-lists (point) -1 1)
         (scan-lists (point) 1 1)))
    (cons (save-excursion
            (while (and (search-backward -left)
                        (eq ?\\ (char-before (point)))))
            (point))
          (save-excursion
            (while (and (search-forward -right)
                        (eq ?\\ (char-before (- (point) (length -right))))))
            (point)))))

(defun ymacs-editor//surround-mark ()
  (setq ymacs-editor-suround-origin-pos nil)
  (let* ((char1 (read-char))
         (from-pair (and (not (region-active-p))
                         (ymacs-editor//surround-get-pair char1))))
    (when-let ((pos (and (not (region-active-p))
                         (ymacs-editor//surround-get-bounds (car from-pair) (cdr from-pair)))))
      (setq ymacs-editor-suround-origin-pos (point))
      (set-mark (car pos))
      (goto-char (cdr pos))
      (activate-mark))
    (list (region-beginning)
          (region-end)
          from-pair
          (ymacs-editor//surround-get-pair (read-char)))))

;;;###autoload
(defun ymacs-editor/change-surround (-beg -end -from-pair -to-pair)
  (interactive (ymacs-editor//surround-mark))
  (unless (equal -from-pair -to-pair)
    (if (equal -beg -end)
        (message "Empty region")
      (let ((from-left (car -from-pair))
            (from-right (cdr -from-pair))
            (to-left (car -to-pair))
            (to-right (cdr -to-pair)))
        (save-excursion
          (goto-char -end)
          (let ((rl (length from-right)))
            (when (equal (buffer-substring-no-properties
                          (max (point-min) (- (point) rl)) (point))
                         from-right)
              (delete-char (- rl))))
          ;; Insert right
          (when to-right (insert to-right))
          (goto-char -beg)
          (let ((ll (length from-left)))
            (when (equal (buffer-substring-no-properties
                          (point) (min (point-max) (+ (point) ll)))
                         from-left)
              (when ymacs-editor-suround-origin-pos
                (cl-decf ymacs-editor-suround-origin-pos ll))
              (delete-char ll)))
          ;; Insert left
          (when to-left
            (when ymacs-editor-suround-origin-pos
              (cl-incf ymacs-editor-suround-origin-pos (length to-left)))
            (insert to-left))))
      ;; Restore original point
      (when ymacs-editor-suround-origin-pos
        (goto-char ymacs-editor-suround-origin-pos)))))
