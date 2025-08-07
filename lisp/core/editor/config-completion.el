;;; -*- lexical-binding: t; -*-

(option! editor-search-directory nil
  "run searcher in this direcotry"
  :type 'string
  :safe #'stringp)

(defvar ymacs-editor-minibuffer-saved-point nil)

(defvar ymacs-editor-ripgrep-map
  (define-key! :map (make-sparse-keymap)
    ("M-." . ymacs-editor/meta-dot-for-ripgrep)))

(defconst ymacs-editor-rg-type-aliases
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

(after! marginalia
  (dolist (catogory '(command function variable file))
    (setf (alist-get catogory marginalia-annotators) '(builtin))))

(after! vertico
  (setq vertico-count 15)
  (setq vertico-cycle t)
  (setq vertico-resize nil)
  (setq vertico-count-format '("%10s " . "%s/%s"))

  (setq completion-styles '(basic partial-completion emacs22))
  (setq completion-category-overrides '((file (styles partial-completion orderless))))

  (add-hook 'rfn-eshadow-update-overlay-hook #'vertico-directory-tidy)

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

  (setq consult--source-recent-dired
        (list :name "Dired Buffer"
              :narrow '(100 . "Dired")
              :hidden t
              :category 'buffer
              :face 'consult-buffer
              :history 'buffer-name-history
              :state #'consult--buffer-state
              :items (lambda ()
                       (consult--buffer-query
                        :sort 'visibility
                        :mode 'dired-mode
                        :as (lambda (-buffer) (buffer-local-value 'default-directory -buffer))))))

  (add-to-list 'consult-buffer-sources 'consult--source-recent-dired 'append)

  ;; (define-advice consult-imenu--items (:before () refresh)
  ;;   (setq imenu-max-item-length (max 80 (- (frame-width) 20)))
  ;;   (when (eq imenu-create-index-function #'semantic-create-imenu-index)
  ;;     (semantic-fetch-tags)))

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
    ("X" . delete-file))

  (setq embark-verbose-indicator-display-action '(display-buffer-in-side-window (side . left)))
  (setq embark-verbose-indicator-excluded-actions
        '(embark-collect
          embark-live embark-export
          embark-cycle embark-act-all embark-keymap-help
          embark-become embark-isearch-forward
          embark-isearch-backward)))

(after! corfu
  (setq tab-always-indent 'complete)

  (setq corfu-auto-prefix 2)
  (setq corfu-cycle t)                ;; Enable cycling for `corfu-next/previous'
  (setq corfu-auto nil)               ;; Enable auto completion
  (setq corfu-separator ?\s)          ;; Orderless field separator
  (setq corfu-quit-at-boundary t)     ;; Never quit at completion boundary
  ;; (setq corfu-quit-no-match nil)      ;; Never quit, even if there is no match
  (setq corfu-preview-current nil)    ;; Disable current candidate preview
  (setq corfu-preselect 'valid)      ;; Preselect the prompt
  ;; (setq corfu-on-exact-match nil)     ;; Configure handling of exact matches
  ;; (setq corfu-scroll-margin 5)        ;; Use scroll margin
  )

(after! corfu-terminal
  (setq corfu-terminal-enable-on-minibuffer nil))
