;;; -*- lexical-binding: t; -*-

(defvar ymacs-editor-view-code-modes
  '((t display-line-numbers-mode
       read-only-mode
       line-number-mode
       column-number-mode
       size-indication-mode
       highlight-indentation-mode)))

(define-minor-mode ymacs-editor-view-code-mode
  "View code"
  :group 'ymacs
  :init-value nil
  (let ((switch (if ymacs-editor-view-code-mode 1 -1)))
    (cl-loop for (condition . modes) in ymacs-editor-view-code-modes
             when (or (eq condition t)
                      (and (symbolp condition) (symbol-value condition)))
             do (dolist (mode modes)
                  (funcall mode switch)))))

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

(defun ymacs-default//tab-bar ()
  `((global menu-item ,(format-mode-line (ymacs-modeline//format--header)) ignore)))

(after! tab-bar
  (setq tab-bar-format '(ymacs-default//tab-bar)))

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
        '("[" (:propertize which-func-current face which-func) "]")))

(after! display-line-numbers
  (setq display-line-numbers-type t)
  (setq-default display-line-numbers-width 2))

;; `whitespace-space' setup
(after! whitespace
  (setq whitespace-global-modes '(text-mode))
  (setq whitespace-style
        '(face tabs space-before-tab indentation empty space-after-tab tab-mark)))
