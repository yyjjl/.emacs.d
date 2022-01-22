;;; -*- lexical-binding: t; -*-

;; (defun ymacs-editor//try-enable-ctags ()
;;   (when (and ymacs-ctags-path
;;              (require 'citre nil t)
;;              (not (file-remote-p default-directory))
;;              (citre-tags-file-path))
;;     ;; disable semantic when using citre
;;     (setq ymacs-editor--inhibit-semantic t)
;;     (unless (eval-when-has-feature! lsp
;;               (bound-and-true-p lsp-mode))
;;       (citre-mode 1)
;;       (ymacs-editor//add-company-backend
;;        'ymacs-editor//company-citre
;;        :after 'company-capf
;;        :remove-capf nil))))

;; (defun ymacs-editor//company-citre (-command &optional -arg &rest _ignored)
;;   "Completion backend of for citre.  Execute COMMAND with ARG and IGNORED."
;;   (interactive (list 'interactive))
;;   (cl-case -command
;;     (interactive (company-begin-backend 'ymacs-editor//company-citre))
;;     (prefix (and (bound-and-true-p citre-mode)
;;                  (or (citre-get-symbol) 'stop)))
;;     (meta (citre-get-property 'signature -arg))
;;     (annotation (concat (citre-capf--get-annotation -arg) ":citre"))
;;     (candidates (all-completions -arg (citre-capf--get-collection -arg)))
;;     (ignore-case (not citre-completion-case-sensitive))))

;; (add-hook 'find-file-hook #'ymacs-editor//try-enable-ctags)

;; (after! dired
;;   (require 'citre-lang-fileref))

;; (after! cc-mode
;;   (require 'citre-lang-c))

;; (after! citre
;;   ;; default fallback to citre-xref-backend
;;   (define-advice xref--create-fetcher (:around (-fn -input -kind -arg) fallback)
;;     (let ((fetcher (funcall -fn -input -kind -arg))
;;           (citre-fetcher
;;            (let ((xref-backend-functions '(citre-xref-backend t)))
;;              (funcall -fn -input -kind -arg))))
;;       (lambda ()
;;         (or (ignore-errors (funcall fetcher))
;;             (condition-case err
;;                 (prog1 (funcall citre-fetcher)
;;                   (message "%s found for `%s' using citre" -kind -input))
;;               (error
;;                (user-error "No %s found for `%s' (%s)" -kind -input (error-message-string err))))))))

;;   ;; (setq citre-enable-capf-integration nil)
;;   (setq citre-ctags-program ymacs-ctags-path)
;;   (setq citre-default-create-tags-file-location 'global-cache)
;;   (setq citre-tags-file-global-cache-dir (expand-cache! "ctags"))
;;   (setq citre-use-project-root-when-creating-tags t)
;;   (setq citre-project-root-function #'ymacs-editor//project-root)

;;   (define-key! :map citre-edit-cmd-buf-map
;;     ("C-c g" . ymacs-editor//insert-git-files)))

;; (after! citre-peek
;;   (define-key! :map citre-peek-keymap
;;     ("C-n" . citre-peek-next-line)
;;     ("C-p" . citre-peek-prev-line)
;;     ("M-n" . citre-peek-next-definition)
;;     ("M-p" . citre-peek-prev-definition)
;;     ("M-N" . citre-peek-move-current-def-down)
;;     ("M-P" . citre-peek-move-current-def-up)
;;     ("n" . citre-peek-chain-forward)
;;     ("p" . citre-peek-chain-backward)
;;     ("b" . citre-peek-prev-branch)
;;     ("f" . citre-peek-next-branch)
;;     ("F" . citre-peek-make-current-def-first)
;;     ("M-." . citre-peek-through)
;;     (("RET" "M-j") . citre-peek-jump)))
