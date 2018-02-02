(setvar!
 cpp-cquery-base-path "~/program/cquery"
 cpp-cquery-path (expand-file-name "build/release/bin/cquery"
                                   cpp-cquery-base-path)
 cpp-has-cmake-p (executable-find "cmake"))

;; C/C++ I need stable version
(require-packages!
 ;; (ggtags :when tags-has-gtags-p)
 cquery
 clang-format
 google-c-style)

(require 'init-cmake)

(defcustom cpp-setup-literally nil
  "Whether to setup project literally"
  :group 'cmake
  :type 'directory
  :safe #'atom)

(defconst cpp--font-lock-keywords
  (eval-when-compile
    `((,(concat "\\<"
                (regexp-opt
                 '("alignas" "alignof" "and" "and_eq" "asm" "auto" "bitand" "bitor" "break" "case"
                   "catch" "class" "compl" "concept" "const" "constexpr" "const_cast" "continue" "decltype"
                   "default" "delete" "do" "double" "dynamic_cast" "else" "enum" "explicit" "export" "extern"
                   "for" "friend" "goto" "if" "import" "inline" "module" "mutable" "namespace" "new"
                   "noexcept" "not" "not_eq" "operator" "or" "or_eq" "private" "protected" "public" "register"
                   "reinterpret_cast" "requires" "return" "short" "signed" "sizeof" "static" "static_assert"
                   "static_cast" "struct" "switch" "template" "this" "thread_local" "throw" "try" "typedef" "typeid"
                   "typename" "union" "using" "virtual" "void" "volatile" "while" "xor" "xor_eq"))
                "\\>")
       . font-lock-keyword-face)
      (,(concat "\\<"
                (regexp-opt '("bool" "float" "double" "int" "long"
                              "char" "char16_t" "char32_t" "wchar_t" "unsigned"))
                "\\>")
       . font-lock-type-face)
      (,(concat "\\<" (regexp-opt '("NULL" "nullptr" "false" "true")) "\\>")
       . font-lock-constant-face))))

(custom-theme-set-faces
 'doom-molokai
 '(cquery-sem-free-var-face ((t (:inherit default))))
 '(cquery-sem-member-var-face
   ((t (:weight bold :inherit default)))))




(defun cpp%cquery-load-config (filename)
  (let (driver options)
    (when (file-exists-p filename)
      (let ((content (split-string (read-file-content! filename)
                                   "\n" :omit-nulls " \t"))
            option)
        (while (setq option (pop content))
          (if (string-prefix-p "# " option)
              (when (string-match-p "^#\s+Driver" option)
                (setq driver (pop content)))
            (push option options)))))
    (cons (or driver "g++") options)))

(defsubst cpp%get-cquery-file (&optional $dir)
  (expand-file-name ".cquery"
                    (locate-dominating-file (or $dir default-directory)
                                            ".cquery")))

(defun cpp/run-cmake ()
  (interactive)
  (if (and (not (process-live-p cpp--cmake-process))
           cpp-cmakelists-directory)
      (progn
        (cpp%maybe-create-build-directory)
        (message "Running cmake for src path %s in build path %s"
                 (abbreviate-file-name cpp-cmakelists-directory)
                 (abbreviate-file-name cpp-build-directory))
        (cpp%run-cmake-internal
         (lambda ()
           (cpp%cquery-setup)
           (message "%s" (cpp%options-to-string)))))
    (message "CMake is running or No CMakeLists.txt has been found.")))

(defun cpp/create-cquery-file ($dir)
  (interactive (list (if current-prefix-arg
                         (read-directory-name "Directory: ")
                       default-directory)))
  (when $dir
    (let ((cquery-file (expand-file-name ".cquery" $dir)))
      (if (file-exists-p cquery-file)
          (message ".cquery already exists")
        (with-temp-buffer
          (erase-buffer)
          (insert cpp--cquery-default-template)
          (write-file cquery-file))
        (message ".cquery created")))))

(defun cpp%cquery-setup ()
  (unless lsp-mode
    (condition-case err
        (lsp-cquery-enable)
      (error (message "%s" (error-message-string err))))
    (add-to-list 'company-backends 'company-lsp)))

(defun cpp%common-cc-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (turn-on-auto-fill)
  (google-set-c-style)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  ;; (c-toggle-auto-newline 1)
  (highlight-indentation-set-offset c-basic-offset)
  (c-toggle-hungry-state 1))

(defun cpp%simple-setup ()
  (setq-local
   compile-command
   '(ignore-errors
      (let ((root (cpp%locate-cmakelists nil nil "Makefile")))
        (if root
            (concat "make -C " root)
          (let ((filename (buffer-file-name))
                (cquery-file (cpp%get-cquery-file)))
            (concat (string-join (cpp%cquery-load-config cquery-file) " ")
                    " "
                    (file-name-nondirectory filename) " -o "
                    (file-name-base filename))))))))

(defun cpp/load-file-in-root ()
  (interactive)
  (cond
   ((file-remote-p default-directory)
    (message "Not support in remove sever !"))
   ((not (executable-find "root"))
    (message "Executable `root' not found !"))
   ((not (buffer-file-name))
    (message "Buffer has no file !"))
   (t
    (let* ((file (buffer-file-name))
           (buffer-name (format "*root:%s*" (buffer-name)))
           (buffer (get-buffer-create buffer-name))
           (proc (get-buffer-process buffer)))
      (if (and proc
               (process-live-p proc)
               (eq (buffer-local-value 'major-mode buffer)
                   'term-mode))
          (progn
            (with-current-buffer buffer
              (term-send-raw-string (format ".X %s\n" file))))
        (kill-buffer buffer)
        (setq buffer
              (term/exec-program "root"
                                 (list "-l" (or file ""))
                                 buffer-name)))
      (pop-to-buffer buffer)))))

(defun cpp/compile ()
  (interactive)
  (let ((command (and cpp-cmakelists-directory
                      (progn
                        (cpp%maybe-create-build-directory)
                        (cpp%get-compile-command cpp-build-directory)))))
    (if command
        (compile command)
      (call-interactively 'compile))))

(defun cpp/gdb (&optional directory)
  (interactive
   (list (expand-file-name
          (read-directory-name
           "Directory: "
           (file-name-as-directory (or (and cpp-cmakelists-directory
                                            cpp-build-directory)
                                       default-directory))
           ""
           :must-match))))
  (unless (featurep 'gud)
    (require 'gud nil :noerror))
  (let ((default-directory directory))
    (call-interactively #'gdb)))

(defun cpp/lsp-restart ()
  (interactive)
  (-when-let* ((buffers (and lsp-mode
                             lsp--cur-workspace
                             (lsp--workspace-buffers lsp--cur-workspace)))
               (proc (lsp--workspace-proc lsp--cur-workspace)))
    (when (and (process-live-p proc)
               (or (y-or-n-p "LSP server is already running kill it? ")
                   (error "LSP server must be killed !!!")))
      (ignore-errors (lsp--shutdown-cur-workspace))
      (sit-for 1))
    (dolist (buffer buffers)
      (with-current-buffer buffer
        (when (and lsp--cur-workspace
                   (not (process-live-p
                         (lsp--workspace-proc lsp--cur-workspace))))
          (setq lsp--cur-workspace nil)
          (lsp--unset-variables))
        (lsp-mode -1)
        (lsp-cquery-enable)))))

(defun cpp%font-lock-setup ()
  (when (or (eq font-lock-maximum-decoration 1)
            (and (listp font-lock-maximum-decoration)
                 (eq (cdr (assoc major-mode
                                 font-lock-maximum-decoration))
                     1)))
    (font-lock-add-keywords nil cpp--font-lock-keywords)))

(defun cpp%setup ()
  "C/C++ only setup"
  ;; (hide-ifdef-mode 1)

  ;; Make a #define be left-aligned
  (setq c-electric-pound-behavior '(alignleft))

  (cpp%font-lock-setup)
  (cpp%simple-setup)

  (unless (or (file-remote-p default-directory)
              (bound-and-true-p cpp-setup-literally)
              (> (buffer-size) core-large-buffer-size))
    (when (setq cpp-cmakelists-directory (cpp%locate-cmakelists))
      (cpp%maybe-create-build-directory))
    (if (file-exists-p (cpp%get-cquery-file))
        (cpp%cquery-setup)
      (cpp%maybe-create-cdb-file))))

;; Do not use `c-mode-hook' and `c++-mode-hook', there is a bug
(defvar-local cpp--initialized-p nil)
(define-hook! cpp|common-setup (c-mode-common-hook)
  (unless cpp--initialized-p
    (setq cpp--initialized-p t)
    (if (>= (string-to-number c-version) 5.33)
        (run-hooks 'prog-mode-hook))

    (cpp%common-cc-setup)

    (unless (buffer-temporary?)
      (unless (or (derived-mode-p 'java-mode)
                  (derived-mode-p 'groovy-mode))
        (cpp%setup)))))

(defun cpp/electric-star ($arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value $arg))
        (insert "*/")
        (backward-char 2)
        (indent-according-to-mode))
    (call-interactively 'c-electric-star)))

(defmacro cpp%cquery-define-find (symbol command)
  `(defun ,(intern (format "cpp/xref-find-%s" symbol)) ()
     (interactive)
     (cquery-xref-find-custom ,command)))

(cpp%cquery-define-find base "$cquery/base")
(cpp%cquery-define-find callers "$cquery/callers")
(cpp%cquery-define-find derived "$cquery/derived")
(cpp%cquery-define-find vars "$cquery/vars")

(defvar cpp-cquery-jump-map
  (define-key! :map (make-sparse-keymap)
    ("d" . cpp/xref-find-derived)
    ("v" . cpp/xref-find-vars)
    ("b" . cpp/xref-find-base)
    ("c" . cpp/xref-find-callers)))

(with-eval-after-load 'cc-mode
  (require 'cquery)

  ;; Smart tab
  (advice-add 'c-indent-line-or-region :around #'core%indent-for-tab)

  (when (>= (string-to-number c-version) 5.33)
    (put 'c++-mode 'derived-mode-parent 'prog-mode)
    (put 'c-mode 'derived-mode-parent 'prog-mode)
    (put 'java-mode 'derived-mode-parent 'prog-mode))

  (define-key! :map c-mode-base-map
    ("*" . cpp/electric-star)
    ("C-c o" . ff-find-other-file)
    ("C-c C-j" . semantic-ia-fast-jump)
    ("C-c C-v" . semantic-decoration-include-visit)
    ("M-n" . flycheck-next-error)
    ("M-p" . flycheck-previous-error))

  (define-key! :map c++-mode-map
    ("C-c b" . clang-format-buffer)
    ("C-c C-b" . clang-format-buffer)
    ("C-c C-l" . cpp/load-file-in-root)
    ("C-c C-SPC" . cquery-select-codeaction)
    ("C-c T" . cpp/cmake-toggle-option)
    ("C-c d" . cpp/cmake-edit-option)
    ("C-c C-c" . cpp/run-cmake)
    ("M-s l" . cquery-code-lens-mode)
    ([f9] . cpp/run-cmake)
    ([f10] . cpp/compile)
    ([f5] . cpp/gdb))
  (define-key c++-mode-map (kbd "C-c j") cpp-cquery-jump-map))

(with-eval-after-load 'cmake-mode
  (define-key cmake-mode-map [f10] 'compile))

(setq cquery-extra-args '("--language-server"))
(with-eval-after-load 'cquery
  ;; need to register snippet capability
  (require 'company-lsp)

  (push 'c++-mode (get 'lsp 'flycheck-modes))
  (push 'c-mode (get 'lsp 'flycheck-modes))
  (setq cquery-executable cpp-cquery-path
        cquery-extra-init-params '(:index (:comments 2 :builtinTypes t)
                                          :cacheFormat "msgpack"))
  (setq cquery-sem-highlight-method 'font-lock))

;; Set term default directory
(when (boundp 'term-default-directory-function-list)
  (add-to-list 'term-default-directory-function-list
               (lambda ()
                 (and cpp-cmakelists-directory
                      cpp-build-directory))))

(cond ((eq font-lock-maximum-decoration t)
       (setq font-lock-maximum-decoration
             '((c++-mode . 1) (c-mode . 1) (t . t))))
      ((listp font-lock-maximum-decoration)
       (dolist (mode '(c++-mode c-mode))
         (let ((cell (assoc mode font-lock-maximum-decoration)))
           (if cell
               (setcdr cell 1)
             (push (cons mode 1) font-lock-maximum-decoration))))))

(provide 'init-cquery)
