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

(after! marginalia
  (dolist (catogory '(command function variable file))
    (setf (alist-get catogory marginalia-annotator-registry) '(builtin))))

(after! selectrum
  (setq selectrum-max-window-height 13)
  (setq selectrum-show-indices t)

  (setq completion-styles '(orderless))
  (setq completion-category-overrides '((file (styles partial-completion orderless))))

  (define-advice selectrum--count-info (:override () fixed-width)
    (let* ((total (length selectrum--refined-candidates))
           (current (1+ (or selectrum--current-candidate-index -1)))
           (width (1+ (floor (log (max 1 total) 10)))))
      (format (format "%%%dd/%%%dd " width width) current total)))

  (define-key! :map selectrum-minibuffer-map
    ([remap delete-backward-char] . ymacs-editor/minibuffer-delete-char)
    ([remap backward-kill-word] . ymacs-editor/minibuffer-delete-word)
    ("M-n" . ymacs-editor//next-history-element)
    ("M-o" . embark-act)
    ("C-c C-o" . embark-export)))

(after! consult
  (require 'embark-consult)

  (define-advice consult-imenu--items (:before () refresh)
    (when (eq imenu-create-index-function #'semantic-create-imenu-index)
      (semantic-fetch-tags)))

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
  ;; (setq xref-show-xrefs-function #'consult-xref)
  ;; (setq xref-show-definitions-function #'consult-xref)

  (consult-customize
   consult-line consult-yank-pop
   :preview-key 'any
   consult-ripgrep
   :keymap ymacs-editor-ripgrep-map)

  (setq consult-async-refresh-delay 0.1)
  (setq consult-async-input-debounce 0.2)
  (setq consult-async-input-throttle 0.2)

  (setq consult-preview-key (kbd "C-l"))
  (setq consult-narrow-key ">")
  (setq consult-project-root-function #'ymacs-editor//project-root-or-default)

  (define-key! :map minibuffer-local-map
    ("C-r" . consult-history)))

(after! embark
  (define-advice embark-collect-snapshot (:override (&optional -initial-view) fix)
    (when (get-buffer "*Embark Collect*")
      (kill-buffer "*Embark Collect*"))
    (when-let ((window (embark--collect "*Embark Collect*" -initial-view :snapshot))
               (buffer (window-buffer window)))
      (ymacs-editor//minibuffer-quit-and-run
        (pop-to-buffer buffer))))

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
