;;; -*- lexical-binding: t; -*-

(declare-function bookmark-get-bookmark-record 'bookmark)

(defvar-local ymacs-editor-prefer-imenu-p nil)

(defvar ymacs-editor-ivy--last-text nil)

(defvar ymacs-editor-ivy-switch-function-list nil)
(defvar ymacs-editor-ivy-extra-help-lines nil)
(defvar ymacs-editor-ivy-display-help-max-width 160)

(defvar ymacs-editor-ivy-display-help-extra-commands
  '(ivy-restrict-to-matches
    ;; delete-blank-lines
    ;; just-one-space
    (counsel-find-file . ivy-magic-read-file-env)))

(defvar ymacs-editor-ivy-display-help-ignore-commands
  '(ymacs-editor/ivy-meta-dot
    counsel-up-directory
    swiper-C-s
    swiper-recenter-top-bottom))

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

;;
;;* Dispaly Help in LV 
;;

(defsubst ymacs-editor//display-keys--collect (-keymap)
  (let (keys)
    (when -keymap
      (cl--map-keymap-recursively
       (lambda (key definition)
         (unless (or (eq (aref key 0) 'remap) ; skip remap
                     (memq definition ymacs-editor-ivy-display-help-ignore-commands))
           (push (cons (key-description key) definition) keys)))
       -keymap))
    keys))

(defsubst ymacs-editor//display-keys--collect-extra ()
  (cl-loop for command in ymacs-editor-ivy-display-help-extra-commands
           for keys = (when (or (not (consp command))
                                (prog1 (eq (ivy-state-caller ivy-last) (car command))
                                  (setq command (cdr command))))
                          (where-is-internal command))
           when keys
           append (cl-loop
                   for key in keys
                   collect
                   (cons (key-description key) command))))

(defsubst ymacs-editor//display-keys--format (keys)
  (cl-loop
   for (definition . items) in (-group-by #'cdr keys)
   collect
   (format "[%s %s]"
           (mapconcat
            (lambda (item)
              (propertize (car item) 'face 'help-key-binding))
            items
            "/")
           (cond ((symbolp definition)
                  definition)
                 ((functionp definition)
                  "<anonymous>")
                 (t "<error>")))))

(defsubst ymacs-editor//display-help--keys ()
  (when-let ((keymap (ivy-state-keymap ivy-last))
             (keys (ymacs-editor//display-keys--format
                    (append (ymacs-editor//display-keys--collect keymap)
                            (ymacs-editor//display-keys--collect-extra)))))
    (let ((max-width (min (frame-width) ymacs-editor-ivy-display-help-max-width))
          (width 0)
          (strings))
      (dolist (key keys)
        (when (> (+ (length key) width) max-width)
          (push "\n" strings)
          (setq width 0))
        (cl-incf width (length key))
        (push key strings))
      (string-join (nreverse strings)))))

(defun ymacs-editor//display-help (&optional -cmd -directory)
  (when (and -cmd (not (stringp -cmd)))
    (setq -cmd (string-join -cmd " ")))

  (when -cmd
    (setq -cmd (propertize -cmd 'face font-lock-doc-face)))

  (let ((max-cmd-length (frame-width)))
    (when (> (length -cmd) max-cmd-length)
      (setq -cmd (concat (substring -cmd 0 max-cmd-length)
                         (propertize "[...]" 'face font-lock-keyword-face)))))

  (setq -directory
        (propertize (or -directory default-directory)
                    'face font-lock-constant-face))

  (let* ((extra-string
          (when ymacs-editor-ivy-extra-help-lines
            (propertize (string-join ymacs-editor-ivy-extra-help-lines "\n")
                        'face 'font-lock-string-face)))
         (key-string (ymacs-editor//display-help--keys))
         (cmd-string
          (when -cmd
            (format "(@%s) %s" -directory -cmd)))
         (help-string
          (concat extra-string
                  (when (and extra-string (or key-string cmd-string)) "\n")
                  key-string
                  (when (and cmd-string key-string) "\n")
                  cmd-string)))
    (unless (string-empty-p help-string)
      (lv-message "%s" help-string))))

(defun ymacs-editor//ivy-re-builder (-str)
  (when (string-prefix-p "=" -str)
    (setq -str (pinyinlib-build-regexp-string (substring -str 1) t nil t)))
  (ivy--regex-plus -str))

;;
;;* Switch Backend
;;

(defmacro ymacs-editor//define-switch (&rest -body)
  (declare (indent 0))
  (let* ((commands (mapcar #'car -body))
         (props-list (mapcar #'cdr -body))
         (switch-fns (--map (intern (format "ymacs-editor//switch-to-%s" it)) commands)))
    `(progn
       ,@(cl-loop
          for (command current-switch-fn props) in (-zip commands switch-fns props-list)
          collect
          `(progn
             (defun ,current-switch-fn (&rest _)
               (interactive)
               ,@(when (plist-get props :save-text)
                   `((setq ymacs-editor-ivy--last-text ivy-text)))
               (ivy-quit-and-run
                 ,(if (plist-get props :save-text)
                      `(,command)
                    `(,command ivy-text))))
             (ivy-add-actions
              ',(or (plist-get props :caller) command)
              ',(cl-loop
                 for (switch-fn . props) in (-zip switch-fns props-list)
                 when (not (eq switch-fn current-switch-fn))
                 collect (list (plist-get props :key) switch-fn (plist-get props :doc)))))))))

(defun ymacs-editor/switch-ivy-backend ()
  (interactive)
  (let* ((caller (ivy-state-caller ivy-last))
         (toggle-fn (cl-loop
                     for (callers . toggle-fn) in ymacs-editor-ivy-switch-function-list
                     when (memq caller callers)
                     return toggle-fn)))
    (unless toggle-fn
      (user-error "No toggle-function defined"))
    (funcall toggle-fn)))

;;
;;* Transformers
;;

(defun ymacs-editor//ivy-package-install-transformer (-string)
  (let ((package (cadr (assoc-string -string package-archive-contents))))
    (concat
     (format "%-30s" -string)
     (when package
       (format " %-16s %-7s %s"
               (propertize (package-version-join (package-desc-version package))
                           'face font-lock-comment-face)
               (propertize (package-desc-archive package)
                           'face font-lock-builtin-face)
               (propertize (package-desc-summary package)
                           'face font-lock-doc-face))))))

(defun ymacs-editor//ivy-switch-buffer-transformer (-string)
  "Transform STR to more readable format."
  (let ((buffer (get-buffer -string)))
    (if (not buffer)
        -string
      (let* ((remote (buffer-local-value 'ymacs-modeline--remote-host buffer))
             (remote (and (not (eq remote 'unset)) remote))
             (face (when buffer
                     (or (when remote 'ivy-remote)
                         (when (not (verify-visited-file-modtime buffer)) 'ymacs-modeline-urgent)
                         (when (and (or (buffer-file-name buffer)
                                        (not (buffer-local-value 'buffer-read-only buffer)))
                                    (buffer-modified-p buffer))
                           'ymacs-modeline-buffer-modified)
                         (cdr (assq (buffer-local-value 'major-mode buffer)
                                    ivy-switch-buffer-faces-alist))))))
        (concat
         (format "%-60s" (ivy-append-face -string face))
         (if (buffer-file-name buffer)
             (when remote
               (propertize (format "(%s) " remote) 'face 'ymacs-modeline-host))
           (propertize
            (buffer-local-value 'default-directory buffer) 'face 'ymacs-modeline-buffer-path)))))))


(defun ymacs-editor//ivy-bookmark-transformer (-string)
  "Transform STR to more readable format."
  (let ((bm (bookmark-get-bookmark-record -string)))
    (concat
     (format "%-40s" (propertize -string 'face 'font-lock-string-face))
     (when bm
       (propertize (format "%-10d" (cdr (assoc 'position bm)))
                   'face 'warning))
     (when bm
       (file-name-nondirectory (cdr (assoc 'filename bm)))))))


(after! ivy
  (add-hook 'ivy-occur-mode-hook #'ymacs-editor//truncate-line)
  (add-hook 'ivy-occur-grep-mode-hook #'ymacs-editor//truncate-line)

  (define-advice ivy-occur-next-error (:around (-fn &rest -args) ensure-visible)
    (if-let (window (or (get-buffer-window (current-buffer))
                        (display-buffer (current-buffer))))
        (with-selected-window window
          (apply -fn -args))
      (apply -fn -args)))

  (advice-add 'ivy--preselect-index :around #'ignore-errors!)
  (advice-add #'ivy--cleanup :before (lambda (&rest _) (lv-delete-window)))

  (define-key! :map ivy-minibuffer-map
    ("C-r" . ivy-reverse-i-search)
    ("C-j" . ivy-immediate-done)
    ("C-M-j" . ivy-done)
    ("M-." . ymacs-editor/ivy-meta-dot)
    ("C-." . ymacs-editor/ivy-meta-dot))

  (ymacs-editor//define-switch
    (swiper :doc "Swiper" :key "s")
    (swiper-isearch :doc "SwiperI" :key "x")
    (swiper-all :doc "SwiperA" :key "a")
    (ymacs-editor//rg :doc "ripgrep" :key "r" :save-text t :caller counsel-rg)
    (counsel-git-grep :doc "gitgrep" :key "g"))

  (ymacs-editor//define-switch
    (ymacs-editor//fzf :doc "fzf" :key "z" :caller counsel-fzf)
    (counsel-git :doc "git" :key "g")
    (counsel-find-file :doc "find file" :key "f"))

  (dolist (caller '(ivy-switch-buffer
                    internal-complete-buffer
                    ivy-switch-buffer-other-window))
    (ivy-configure caller
      :display-transformer-fn
      #'ymacs-editor//ivy-switch-buffer-transformer))

  (ivy-configure 'package-install
    :display-transformer-fn
    #'ymacs-editor//ivy-package-install-transformer)
  (ivy-configure 'counsel-bookmark
    :display-transformer-fn
    #'ymacs-editor//ivy-bookmark-transformer)

  (add-to-list 'ivy-hooks-alist '(t . ymacs-editor//display-help))

  (setq ivy-read-action-function #'ivy-hydra-read-action)
  (setf (alist-get 't ivy-format-functions-alist) #'ivy-format-function-arrow)
  (setq ivy-height 13)
  (setq ivy-action-wrap t)
  (setq ivy-count-format "(%d/%d) ")
  (setq ivy-extra-directories '("./"))
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-fixed-height-minibuffer t)
  (setq ivy-re-builders-alist '((t . ymacs-editor//ivy-re-builder)))
  (setq ivy-use-virtual-buffers t)
  (setq ivy-virtual-abbreviate 'full)
  (setq ivy-use-selectable-prompt t)
  (setq ivy-on-del-error-function #'ignore))

(after! swiper
  (setq swiper-stay-on-quit t))

(after! counsel
  (define-advice counsel--async-command (:before (-cmd &rest _) show-help)
    (ymacs-editor//display-help -cmd))

  (add-to-list 'counsel-compile-local-builds #'ymacs-editor//default-compile-command t)

  (setq counsel-yank-pop-separator "\n------------------------------------------------------------\n")
  (setq counsel-find-file-at-point t)
  (setq counsel-find-file-ignore-regexp
        (concat
         ;; file names beginning with # or .
         "\\(?:\\`[#]\\)"
         ;; file names ending with # or ~
         "\\|\\(?:[#~]\\'\\)"))

  (setq counsel-compile-make-args "-k -j"))
