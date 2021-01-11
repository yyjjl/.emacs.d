;; -*- lexical-binding:t -*-

;;;###autoload
(defun ymacs-web/load-in-repl ()
  (interactive)

  (when (file-remote-p default-directory)
    (user-error "Not support in remove sever !"))
  (unless (executable-find "node")
    (user-error "Executable `root' not found !"))
  (unless (buffer-file-name)
    (user-error "Buffer has no file !"))

  (let ((file (buffer-file-name)))
    (ymacs-term//exec-program-reuse-buffer
     (concat "Node:" (buffer-name))
     "node" (list "-l" (or file ""))
     :-callback
     (lambda () (ymacs-term//send-string (format ".load %s\n" file))))))

;;;###autoload
(defun ymacs-web/smart-kill ()
  "It's a smarter kill function for `web-mode'."
  (interactive)
  (cond
   ;; Kill all content wrap by <% ... %> when right is <%
   ((and (looking-at "<%")
         (when-let (pos (save-excursion (search-forward "%>" nil t)))
           (kill-region (point) pos)
           (message "Kill <%% ... %%>")
           t)))
   ;; Kill string if current pointer in string area.
   ((paredit-in-string-p)
    (paredit-kill-line-in-string)
    (message "Paredit kill"))
   ;; Kill element if no content in tag.
   ((and (looking-at "\\s-*?</")
         (looking-back "<[[:alnum:]]+\\s-*?>\\s-*?" (line-beginning-position)))
    (web-mode-element-kill 1)
    (message "Delete element"))
   ;; Kill whitespace in tag.
   ((looking-at "\\s-+>")
    (delete-horizontal-space)
    (message "Delete whitespace"))
   ;; Kill element if in tag.
   ((and (looking-at "\\s-*?[[:alnum:]]*>")
         (looking-back "</?[[:alnum:]]*\\s-*?" (line-beginning-position)))
    (web-mode-element-kill 1)
    (message "Kill element"))
   ;; Kill attributes if point in attributes area.
   ((when-let ((beg (web-mode-attribute-beginning-position))
               (end (web-mode-attribute-end-position)))
      (and (>= (point) beg) (<= (point) end)))
    (web-mode-attribute-kill)
    (message "Kill attribute"))
   ;; Kill attributes if only space between point and attributes start.
   ((and (looking-at "\\s-+")
         (save-excursion
           (let ((pos (search-forward-regexp "\\s-+" nil t)))
             (when (equal pos (web-mode-attribute-beginning-position))
               (web-mode-attribute-kill)
               (message "Kill attribute")
               t)))))
   ;; Kill if not inside tag
   ((not (web-mode-tag-beginning-position))
    (paredit-kill)
    (message "Paredit kill"))
   (t (message "Nothing to do"))))
