;; -*- lexical-binding: t -*-

(load (expand! "hooks-builtin") nil t)

(defun ymacs-editor//generic-setup ()
  (company-mode 1)
  (hl-line-mode 1)
  (display-fill-column-indicator-mode 1)

  (page-break-lines-mode 1)

  (when (not (derived-mode-p 'org-mode))
    (hl-todo-mode 1))

  (setq show-trailing-whitespace t)
  (setq indicate-empty-lines t))

(define-hook! ymacs-editor//generic-text-mode-setup (text-mode-hook)
  (whitespace-mode 1)

  (ymacs-editor//generic-setup))

(define-hook! ymacs-editor//generic-prog-mode-setup (prog-mode-hook)
  (condition-case err
      (hs-minor-mode 1)
    (user-error (message "%s" (error-message-string err))))

  (ymacs-editor//generic-setup))

(after! ivy
  (add-hook 'ivy-occur-mode-hook #'ymacs-default//truncate-line)
  (add-hook 'ivy-occur-grep-mode-hook #'ymacs-default//truncate-line)
  (advice-add #'ivy--cleanup :before (lambda (&rest _) (lv-delete-window)))

  (define-advice ivy-occur-next-error (:around (-fn &rest -args) ensure-visible)
    (if-let (window (or (get-buffer-window (current-buffer))
                        (display-buffer (current-buffer))))
        (with-selected-window window
          (apply -fn -args))
      (apply -fn -args)))

  (advice-add 'ivy--preselect-index :around #'ignore-errors!))

(after! counsel
  (define-advice counsel--async-command (:before (-cmd &rest _) show-help)
    (ymacs-editor//display-help -cmd)))

(after! company-capf
  (advice-add 'company-capf :around #'ignore-errors!))

(after! yasnippet
  (define-advice yas-next-field-or-maybe-expand (:around (-fn &rest -args) expand-local)
    (or (ymacs-editor//try-expand-local-snippets)
        (apply -fn -args)))

  (after! org
    (advice-add
     #'org-cycle
     :around #'yas-next-field-or-maybe-expand@expand-local)))

(after! multiple-cursors
  (define-advice multiple-cursors-mode (:before (&rest _) disable-iedit)
    (when (bound-and-true-p iedit-mode)
      (iedit-mode -1))))

(after! iedit
  (define-advice iedit-mode (:before (&rest _) disable-mc)
    (when (bound-and-true-p multiple-cursors-mode)
      (multiple-cursors-mode -1))))

(after! ace-window
  (define-advice aw-update (:override () ignore-on)
    (let ((aw-ignore-on t)
          (aw-ignore-current))
      (avy-traverse
       (avy-tree (remove (minibuffer-window) (aw-window-list)) aw-keys)
       (lambda (path leaf)
         (set-window-parameter
          leaf 'ace-window-path
          (propertize (apply #'string (reverse path)) 'face 'aw-mode-line-face)))))))
