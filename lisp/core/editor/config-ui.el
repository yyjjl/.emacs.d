;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-view-code-modes
  '((t display-line-numbers-mode
       read-only-mode
       indent-bars-mode)))

(defvar-local ymacs-editor-view-code--saved-mode-states 'unset)

(defsubst ymacs-editor//view-code-mode-enabled-p (-mode)
  (if (eq -mode 'read-only-mode)
      buffer-read-only
    (and (boundp -mode) (symbol-value -mode))))

(define-minor-mode ymacs-editor-view-code-mode
  "View code"
  :group 'ymacs
  :init-value nil
  (if ymacs-editor-view-code-mode
      (when (eq ymacs-editor-view-code--saved-mode-states 'unset)
        (setq ymacs-editor-view-code--saved-mode-states nil)
        (cl-loop for (condition . modes) in ymacs-editor-view-code-modes
                 when (or (eq condition t)
                          (and (symbolp condition)
                               (boundp condition)
                               (symbol-value condition)))
                 do (dolist (mode modes)
                      (push (cons mode (ymacs-editor//view-code-mode-enabled-p mode))
                            ymacs-editor-view-code--saved-mode-states)
                      (funcall mode 1))))
    (unless (eq ymacs-editor-view-code--saved-mode-states 'unset)
      (dolist (state ymacs-editor-view-code--saved-mode-states)
        (funcall (car state) (if (cdr state) 1 -1)))
      (setq ymacs-editor-view-code--saved-mode-states 'unset))))

(defun ymacs-editor//pulse (&rest _)
  (xref-pulse-momentarily))

(dolist (cmd '(recenter-top-bottom
               other-window
               next-error
               ace-window
               pop-to-mark-command
               pop-global-mark
               ymacs-editor/goto-last-point))
  (advice-add cmd :after #'ymacs-editor//pulse))

(define-hook! ymacs-editor//indirect-buffer-setup (clone-indirect-buffer-hook)
  (when (derived-mode-p 'prog-mode 'text-mode)
    (ymacs-editor-view-code-mode 1)))



(dolist (item '((smerge-mode "SMerge ")
                (view-mode "View ")
                (overwrite-mode ("" overwrite-mode " "))
                (auto-fill-function "Fill ")
                (visual-line-mode "Wrap ")
                (isearch-mode isearch-mode)
                (next-error-follow-minor-mode "Fol ")))
  (add-to-list 'mode-line-misc-info item))

(after! tab-line
  (setq tab-line-left-button "<")
  (setq tab-line-right-button ">")
  (setq tab-line-close-button-show nil)
  (setq tab-line-new-button-show nil)
  (setq tab-line-tab-name-function
        (lambda (buffer &optional _buffers)
          (format "[%s]" (buffer-name buffer))))
  (setq tab-line-separator nil)
  (setq tab-line-tabs-function #'ymacs-popup//get-term-buffer-list))

(setq which-key-dont-use-unicode t)
(after! which-key
  (setq which-key-show-prefix 'left)
  (setq which-key-popup-type 'side-window)
  (setq which-key-allow-imprecise-window-fit nil)
  (setq which-key-show-remaining-keys t))

(after! which-func
  (setq which-func-format
        '("<" (:propertize which-func-current face which-func) ">")))

(after! display-line-numbers
  (setq display-line-numbers-type t)
  (setq-default display-line-numbers-width 2))

;; `whitespace-space' setup
(after! whitespace
  (setq whitespace-global-modes '(text-mode))
  (setq whitespace-style
        '(face tabs space-before-tab indentation empty space-after-tab tab-mark)))
