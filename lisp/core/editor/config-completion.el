;;; -*- lexical-binding: t; -*-

(option! editor-search-directory nil
  "run searcher in this direcotry"
  :type 'string
  :safe #'stringp)

(defvar ymacs-editor-minibuffer-saved-point nil)
(defvar ymacs-editor-minibuffer--last-input nil)
(defvar ymacs-editor-minibuffer--last-directory nil)

(defvar ymacs-editor-ripgrep-map
  (define-key! :map (make-sparse-keymap)
    ("M-." . ymacs-editor/meta-dot-for-ripgrep)))

(defvar ymacs-editor-rg-type-aliases
  (eval-when-compile
    (when ymacs-ripgrep-path
      (condition-case err
          (append
           (--map
            (-let* (((type alias) (split-string it ":" :omit-nulls)))
              (cons (string-trim type)
                    (mapcar #'string-trim (split-string alias "," :omit-nulls))))
            (-> ymacs-ripgrep-path
                (concat " --type-list")
                shell-command-to-string
                (split-string "\n" :omit-nulls)))
           '(("all" "all defined type aliases") ;; rg --type=all
             ("everything" "*")))
        (error (message "%s" err))))))

(defmacro ymacs-editor//minibuffer-quit-and-run (&rest -body)
  "Quit the minibuffer and run BODY afterwards."
  (declare (indent 0))
  `(progn
     (put 'quit 'error-message "")
     (run-at-time nil nil
                  (lambda ()
                    (put 'quit 'error-message "Quit")
                    (with-demoted-errors "Error: %S"
                      ,@-body)))
     (abort-recursive-edit)))

(defun ymacs-editor//vertico-directory-tidy ()
  "Tidy shadowed file name, see `rfn-eshadow-overlay'."
  (when (and (eq this-command #'self-insert-command)
             (bound-and-true-p rfn-eshadow-overlay)
             (overlay-buffer rfn-eshadow-overlay)
             (= (point) (point-max))
             (or (>= (- (point) (overlay-end rfn-eshadow-overlay)) 2)
                 (eq ?/ (char-before (- (point) 2)))))
    (delete-region (overlay-start rfn-eshadow-overlay) (overlay-end rfn-eshadow-overlay))))

(after! marginalia
  (dolist (catogory '(command function variable file))
    (setf (alist-get catogory marginalia-annotator-registry) '(builtin))))

(after! vertico
  (setq vertico-count 13)
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (setq vertico-count-format '("%10s " . "%s/%s"))

  (setq completion-styles '(orderless))
  (setq completion-category-overrides '((file (styles partial-completion orderless))))

  (add-hook 'rfn-eshadow-update-overlay-hook #'ymacs-editor//vertico-directory-tidy)

  (define-key! :map vertico-map
    ([remap delete-backward-char] . ymacs-editor/minibuffer-delete-char)
    ([remap backward-kill-word] . ymacs-editor/minibuffer-delete-word)
    ("M-9" . vertico-quick-jump)
    ("M-i" . vertico-insert)
    ("M-n" . ymacs-editor//next-history-element)
    ("M-o" . embark-act)
    ("C-c C-o" . embark-export))

  (define-key! :map minibuffer-mode-map
    ("M-n" . ymacs-editor//next-history-element)
    ("M-o" . embark-act)
    ("C-c C-o" . embark-export)))

(after! consult
  (require 'embark-consult)

  (defvar consult--source-recent-dired
    (list :name "Dired Buffer"
          :narrow '(100 . "Dired")
          :hidden t
          :category 'buffer
          :face 'consult-buffer
          :history 'buffer-name-history
          :state #'consult--buffer-state
          :items (lambda ()
                   (consult--buffer-query :sort 'visibility :mode 'dired-mode :as #'buffer-name))))

  (add-to-list 'consult-buffer-sources 'consult--source-recent-dired 'append)

  (define-advice consult-imenu--items (:before () refresh)
    (setq imenu-max-item-length (max 80 (- (frame-width) 20)))
    (when (eq imenu-create-index-function #'semantic-create-imenu-index)
      (semantic-fetch-tags)))

  (define-advice consult--async-process (:around (-fn -async -builder &rest -props) display-command)
    (let ((new-builder
           (lambda (action)
             (let ((cmd (funcall -builder action)))
               (ymacs-editor//display-help nil (string-join (car cmd) " "))
               cmd))))
      (apply -fn -async new-builder -props)))

  ;;  configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  ;; tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)

  (setq completion-in-region-function #'consult-completion-in-region)
  ;; Use Consult to select xref locations with preview
  ;; (setq xref-show-xrefs-function #'consult-xref)
  ;; (setq xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-line
   :preview-key 'any
   consult-yank-pop
   :preview-key "C-y"
   consult-ripgrep
   :keymap ymacs-editor-ripgrep-map)

  (setq consult-async-refresh-delay 0.1)
  (setq consult-async-input-debounce 0.2)
  (setq consult-async-input-throttle 0.2)
  (setq consult-fontify-max-size (* 1024 16))

  (setq consult-preview-key "C-l")
  (setq consult-narrow-key ">")
  (setq consult-project-root-function #'ymacs-editor//project-root-or-default)

  (define-key! :map minibuffer-mode-map
    ("C-r" . consult-history)))

(after! embark
  (define-key! :map embark-file-map
    ("d" (defun ymacs-embark/open-directory (-file)
           (dired (file-name-directory -file))))
    ("X" . delete-file)))
