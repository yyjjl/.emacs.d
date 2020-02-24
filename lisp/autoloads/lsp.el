;; -*- lexical-binding:t -*-

(defun lsp//ivy-format-symbol-match (-match)
  "Convert the (hash-valued) -MATCH returned by `lsp-mode` into a candidate string."
  (let ((container-name (gethash "containerName" -match))
        (name (gethash "name" -match)))
    (if (or (null container-name)
            (string-empty-p container-name))
        name
      (format "%s.%s" container-name name))))

(defun lsp//ivy-workspace-symbol-action (-candidate)
  "Jump to selected -CANDIDATE."
  (-let* (((&hash "uri" "range" (&hash "start" (&hash "line" "character")))
           (gethash "location" -candidate)))
    (find-file (lsp--uri-to-path uri))
    (goto-char (point-min))
    (forward-line line)
    (forward-char character)))

(ivy-set-display-transformer 'lsp/ivy-workspace-symbol #'lsp//ivy-format-symbol-match)

;;;###autoload
(defun lsp/ivy-workspace-symbol (-arg)
  "`ivy' for lsp workspace/symbol.
When called with prefix -ARG, search for all symbols of the current workspaces."
  (interactive "P")
    (let ((current-request-id nil)
          (workspaces (if -arg
                          (-> (lsp-session)
                              lsp-session-folder->servers
                              ht-values
                              -flatten
                              -uniq)
                        (lsp-workspaces))))
      (ivy-read
       (concat (if -arg "Global " "")
               "Workspace symbol: ")
       (lambda (user-input)
         (with-lsp-workspaces workspaces
           (let ((request (lsp-make-request "workspace/symbol" (list :query user-input))))
             (when current-request-id
               (lsp--cancel-request current-request-id))
             (setq current-request-id (plist-get request :id))
             (lsp-send-request-async request #'ivy-update-candidates
                                     :mode 'detached)))
         nil)
       :dynamic-collection t
       :require-match t
       :initial-input nil
       :action #'lsp//ivy-workspace-symbol-action
       :caller 'lsp/ivy-workspace-symbol)))

;;;###autoload
(defun lsp/remove-session-folder (-remove-invalid)
  (interactive "P")
  (require 'dired)
  (let* ((session (lsp-session))
         invalid-folders
         valid-folders)
    (cl-loop for folder in (lsp-session-folders session)
             if (file-exists-p folder)
             do (push folder valid-folders)
             else do (push folder invalid-folders))
    (if -remove-invalid
        (if invalid-folders
            (when (dired-mark-pop-up
                   " *lsp-remove*" 'delete invalid-folders 'yes-or-no-p
                   "Remove these folders ")
              (setf (lsp-session-folders (lsp-session)) valid-folders)
              (lsp--persist-session (lsp-session)))
          (message "Nothing to remove."))
      (let ((folder (completing-read "Folder to remove" valid-folders nil t)))
        (setf (lsp-session-folders (lsp-session)) (delete folder valid-folders))
        (lsp--persist-session (lsp-session))))))
