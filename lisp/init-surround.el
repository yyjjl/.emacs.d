(defvar core-surround-pair-alist
  '(("()90" . ("(" . ")"))
    ("{}" . ("{" . "}"))
    ("[]" . ("[" . "]"))
    ("`" . ("`" . "'"))
    ("<>,." . (lambda (_)
                (let ((tag (read-string "Tag: ")))
                  (cons (concat "<" tag ">")
                        (concat "</" tag ">")))))
    ("\\" . (lambda (_)
              (let ((pair (core//surround-get-pair (read-char))))
                (when pair
                  (cons (concat "\\" (car pair))
                        (concat "\\" (cdr pair)))))))
    ("b" . (lambda (_)
             (let ((env (read-string "environment: ")))
               (when env
                 (cons (concat "\\begin{" env "}")
                       (concat "\\end{" env "}"))))))))
(defvar core--suround-origin-pos nil)

(defun core//surround-get-pair (-char)
  "Get pair from -CHAR"
  (let ((pair (cl-loop for (keys . pair) in core-surround-pair-alist
                       when (member -char (string-to-list keys))
                       return pair)))
    (cond ((functionp pair) (funcall pair -char)) ; function
          (pair pair)                             ; normal
          ((eq -char 13) nil)
          (t (cons (char-to-string -char)
                   (char-to-string -char))))))

(defun core//surround-get-bounds (-left -right)
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

(defun core//surround-mark ()
  (setq core--suround-origin-pos nil)
  (let* ((char1 (read-char))
         (from-pair (and (not (region-active-p))
                         (core//surround-get-pair char1))))
    (when-let ((pos (and (not (region-active-p))
                         (core//surround-get-bounds (car from-pair)
                                                    (cdr from-pair)))))
      (setq core--suround-origin-pos (point))
      (set-mark (car pos))
      (goto-char (cdr pos))
      (activate-mark))
    (list (region-beginning)
          (region-end)
          from-pair
          (core//surround-get-pair (read-char)))))

(defun core/change-surround (-beg -end -from-pair -to-pair)
  (interactive (core//surround-mark))
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
              (when core--suround-origin-pos
                (decf core--suround-origin-pos ll))
              (delete-char ll)))
          ;; Insert left
          (when to-left
            (when core--suround-origin-pos
              (incf core--suround-origin-pos (length to-left)))
            (insert to-left))))
      ;; Restore original point
      (when core--suround-origin-pos
        (goto-char core--suround-origin-pos)))))

(global-set-key (kbd "M-'") #'core/change-surround)

(provide 'init-surround)
