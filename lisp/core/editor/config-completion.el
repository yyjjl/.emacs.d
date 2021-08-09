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

(after! selectrum
  (setq selectrum-fix-vertical-window-height t)
  (setq selectrum-max-window-height 13)
  (setq selectrum-show-indices t)
  (setq selectrum-count-style 'current/matches)

  (setq completion-styles '(orderless))
  (setq completion-category-overrides '((file (styles partial-completion orderless))))

  (define-key! :map selectrum-minibuffer-map
    ("M-n" . ymacs-editor//next-history-element)
    ("M-o" . embark-act)
    ("C-c C-o" . embark-export)))

(after! consult
  (require 'embark-consult)

  (define-advice consult--command-builder (:around (-fn -builder) display-command)
    (let ((cmd-fn (funcall -fn -builder)))
      (lambda (input)
        (let ((cmd (funcall cmd-fn input)))
          (ymacs-editor//display-help nil (string-join cmd " "))
          cmd))))

  ;;  configure the register formatting. This improves the register
  ;; preview for `consult-register', `consult-register-load',
  ;; `consult-register-store' and the Emacs built-ins.
  (setq register-preview-delay 0)
  (setq register-preview-function #'consult-register-format)
  ;; tweak the register preview window.
  ;; This adds thin lines, sorting and hides the mode line of the window.
  (advice-add #'register-preview :override #'consult-register-window)
  ;; replace `completing-read-multiple' with an enhanced version.
  (advice-add #'completing-read-multiple :override #'consult-completing-read-multiple)

  ;; Use Consult to select xref locations with preview
  (setq xref-show-xrefs-function #'consult-xref)
  (setq xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-line consult-yank-pop
   :preview-key 'any
   consult-ripgrep
   :keymap ymacs-editor-ripgrep-map)

  (setq consult-async-refresh-delay 0.1)
  (setq consult-async-input-debounce 0.05)
  (setq consult-async-input-throttle 0.05)

  (setq consult-preview-key (kbd "C-l"))
  (setq consult-narrow-key ">")
  (setq consult-project-root-function #'ymacs-editor//project-root-or-default)

  (define-key! :map minibuffer-local-map
    ("C-r" . consult-history)))

(after! embark
  (defun ymacs-editor//embark-which-key-indicator ()
    (lambda (&optional -keymap _targets -prefix)
      (unless (null -keymap)
        (which-key--show-keymap
         "Embark" (if -prefix (lookup-key -keymap -prefix) -keymap)
         nil nil t))))

  (define-advice embark-consult-export-occur (:around (-fn -lines) fix)
    (let ((buffer (funcall -fn -lines)))
      (with-current-buffer buffer
        (setq occur-highlight-regexp ""))
      buffer))

  (setq embark-indicators
        '(ymacs-editor//embark-which-key-indicator
          embark-minimal-indicator
          embark-highlight-indicator
          embark-isearch-highlight-indicator)))
