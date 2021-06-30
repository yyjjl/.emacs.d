;;; -*- lexical-binding: t; -*-

(option! universal-catgs-smart-git-integration t
  "whether to filtering file using git when generating ctags for project"
  :type 'boolean)

(option! universal-ctags-command-template '(("kinds-all" . "'*'") ("fields" . "'*'") ("extras" . "'*'"))
  "command template for universal ctags"
  :type '(alist))

(defsubst ymacs-editor//build-ctags-command (&optional -files -output-file)
  (format "%s %s %s -R %s"
          ymacs-ctags-path
          (mapconcat (lambda (option)
                       (format "--%s=%s" (car option) (cdr option)))
                     ymacs-universal-ctags-command-template
                     " ")
          (if -output-file
              (concat "-f " (shell-quote-argument -output-file))
            "")
          (if -files
              (mapconcat #'shell-quote-argument -files " ")
            ".")))

(defun ymacs-editor//try-enable-ctags ()
  (when (and ymacs-ctags-path
             (require 'citre nil t)
             (citre-tags-file-path))
    (citre-mode 1)))

(defun ymacs-editor//company-citre (-command &optional -arg &rest _ignored)
  "Completion backend of for citre.  Execute COMMAND with ARG and IGNORED."
  (interactive (list 'interactive))
  (cl-case -command
    (interactive (company-begin-backend 'ymacs-editor//company-citre))
    (prefix (and (bound-and-true-p citre-mode)
                 (or (citre-get-symbol) 'stop)))
    (meta (citre-get-property 'signature -arg))
    (annotation (concat (citre-capf--get-annotation -arg) ":citre"))
    (candidates (seq-take (citre-capf--get-collection -arg) 3))
    (ignore-case citre-completion-case-sensitive)))

(after! citre
  ;; default fallback to citre-xref-backend
  (define-advice xref--create-fetcher (:around (-fn -input -kind -arg) fallback)
    (let ((fetcher (funcall -fn -input -kind -arg))
          (citre-fetcher
           (let ((xref-backend-functions '(citre-xref-backend t)))
             (funcall -fn -input -kind -arg))))
      (lambda ()
        (or (ignore-errors (funcall fetcher))
            (condition-case err
                (prog1 (funcall citre-fetcher)
                  (message "%s found for `%s' using citre" -kind -input))
              (error
               (user-error "No %s found for `%s' (%s)" -kind -input (error-message-string err))))))))

  (setq citre-project-root-function #'ymacs-editor//project-root))

(after! citre-peek
  (define-key! :map citre-peek-keymap
    ("C-n" . citre-peek-next-line)
    ("C-p" . citre-peek-prev-line)
    ("M-n" . citre-peek-next-definition)
    ("M-p" . citre-peek-prev-definition)
    ("M-N" . citre-peek-move-current-def-down)
    ("M-P" . citre-peek-move-current-def-up)
    ("n" . citre-peek-chain-forward)
    ("p" . citre-peek-chain-backward)
    ("b" . citre-peek-prev-branch)
    ("f" . citre-peek-next-branch)
    ("F" . citre-peek-make-current-def-first)
    ("M-." . citre-peek-through)
    (("RET" "M-j") . citre-peek-jump)))
