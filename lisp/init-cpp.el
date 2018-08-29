;; -*- lexical-binding:t -*-

(setvar!
 cpp-ccls-base-path (expand-var! "ccls")
 cpp-ccls-path (expand-file-name "build/ccls"
                                   cpp-ccls-base-path)
 cpp-has-cmake-p (executable-find "cmake")
 cpp-has-ccls-p (file-exists-p cpp-ccls-base-path))

(require-packages!
 (ggtags :when env-has-gtags-p)
 (lsp-mode :when cpp-has-ccls-p)
 (company-lsp :when cpp-has-ccls-p)
 (ccls :when cpp-has-ccls-p)
 clang-format
 google-c-style)

(require 'init-cpp-ccls)

(defvar gud-chdir-before-run t)

(defcustom cpp-setup-literally nil
  "Whether to setup project literally"
  :group 'cmake
  :type 'directory
  :safe #'booleanp)

(defconst cpp--font-lock-keywords
  (eval-when-compile
    `((,(concat "\\<"
                (regexp-opt
                 '("alignas" "alignof" "and" "and_eq" "asm"
                   "auto" "bitand" "bitor" "break" "case"
                   "catch" "class" "compl" "concept" "const"
                   "constexpr" "const_cast" "continue" "decltype"
                   "default" "delete" "do" "double" "dynamic_cast"
                   "else" "enum" "explicit" "export" "extern"
                   "for" "friend" "goto" "if" "import" "inline" "module"
                   "mutable" "namespace" "new"
                   "noexcept" "not" "not_eq" "operator" "or" "or_eq"
                   "private" "protected" "public" "register"
                   "reinterpret_cast" "requires" "return" "short"
                   "signed" "sizeof" "static" "static_assert"
                   "static_cast" "struct" "switch" "template" "this"
                   "thread_local" "throw" "try" "typedef" "typeid"
                   "typename" "union" "using" "virtual" "void"
                   "volatile" "while" "xor" "xor_eq"))
                "\\>")
       . font-lock-keyword-face)
      (,(concat "\\<"
                (regexp-opt '("bool" "float" "double" "int" "long"
                              "char" "char16_t" "char32_t" "wchar_t"
                              "unsigned"))
                "\\>")
       . font-lock-type-face)
      (,(concat "\\<" (regexp-opt '("NULL" "nullptr" "false" "true")) "\\>")
       . font-lock-constant-face))))




(defun cpp//compile-command (-dir)
  "Return suitable compile command for current project"
  (cond
   ((bound-and-true-p projectile-project-compilation-cmd)
    projectile-project-compilation-cmd)
   ((file-exists-p (expand-file-name "build.ninja" -dir))
    (concat "ninja -C " -dir))
   ((file-exists-p (expand-file-name "Makefile" -dir))
    (concat "make --no-print-directory -C " -dir))
   (t nil)))



;; Setup

(defun cpp//common-cc-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  ;; (highlight-indentation-mode 1)
  ;; (electric-operator-mode 1)
  (electric-indent-local-mode -1)
  ;; (turn-on-auto-fill)
  (google-set-c-style)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  ;; (c-toggle-auto-newline 1)
  (highlight-indentation-set-offset c-basic-offset)
  (c-toggle-hungry-state 1)
  (c-toggle-electric-state -1))

(defun cpp//font-lock-setup ()
  (when (or (eq font-lock-maximum-decoration 1)
            (and (listp font-lock-maximum-decoration)
                 (eq (cdr (assoc major-mode
                                 font-lock-maximum-decoration))
                     1)))
    (font-lock-add-keywords nil cpp--font-lock-keywords)))

(defun cpp//setup ()
  (if (or (not cpp-has-cmake-p)
          (not cpp-has-ccls-p)
          (file-remote-p default-directory)
          (bound-and-true-p cpp-setup-literally)
          (> (buffer-size) core-large-buffer-size))
      (progn
        (when env-has-gtags-p
          (ggtags-mode 1))
        ;; (setq completion-at-point-functions nil)
        (flycheck-mode -1))
    (setq cpp-cmake-project-root (cpp-cmake//locate-cmakelists))
    (if (or (file-exists-p (cpp-ccls//dot-ccls-path))
            (and cpp-cmake-project-root (file-exists-p (cpp-cmake//cdb-path))))
        (lsp//enable ccls :success (cpp-ccls//setup))
      (when cpp-cmake-project-root
        (message "Need run `cpp/config-project' to setup ccls server")))))

;; Do not use `c-mode-hook' and `c++-mode-hook', there is a bug
(defvar-local cpp--initialized-p nil)
(define-hook! cpp|common-setup (c-mode-common-hook)
  (unless cpp--initialized-p
    (setq cpp--initialized-p t)
    (if (>= (string-to-number c-version) 5.33)
        (run-hooks 'prog-mode-hook))

    (cpp//common-cc-setup)

    (unless (or (derived-mode-p 'java-mode)
                (derived-mode-p 'groovy-mode))
      ;; (hide-ifdef-mode 1)
      ;; Make a #define be left-aligned
      (setq c-electric-pound-behavior '(alignleft))
      (cpp//font-lock-setup)

      (unless (buffer-temporary-p)
        (add-transient-hook!
            (hack-local-variables-hook :local t :name cpp//setup-interal)
          (cpp//setup))))))



(defun cpp/config-project (&optional -set-options)
  (interactive "P")
  (if (not cpp-cmake-project-root)
      (message "CMakeLists.txt hasn't been found.")
    (cpp-cmake//run-cmake-internal
     (lambda (buffer)
       (with-current-buffer buffer
         (unless cpp-setup-literally
           (let ((cdb-path (cpp-cmake//cdb-path)))
             (when (not (file-exists-p cdb-path))
               (error "Can not find compile_commands.json"))
             (let ((default-directory cpp-cmake-project-root))
               (make-symbolic-link cdb-path "compile_commands.json"
                                   :ok-if-already-exists)))
           (when cpp-has-ccls-p
             (lsp//enable ccls :success (cpp-ccls//setup)))))))
    (when -set-options
      (cpp-cmake//set-cmake-options
       (lambda (_)
         (message "Finish setting cmake options"))))))

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
           (buffer-name (format "root:%s" (buffer-name)))
           (buffer (get-buffer-create buffer-name))
           (proc (get-buffer-process buffer)))
      (unless (and proc
                   (process-live-p proc)
                   (eq (buffer-local-value 'major-mode buffer)
                       'term-mode))
        (kill-buffer buffer)
        (setq buffer
              (term//exec-program "root" (list "-l" (or file "")) buffer-name)))
      (when buffer
        (with-current-buffer buffer
          (term-send-raw-string (format ".X %s\n" file)))
        (term//pop-to-buffer buffer))))))

(defun cpp/compile ()
  (interactive)
  (with-temp-env! (cpp-cmake//config-env)
    (let ((command (and cpp-cmake-project-root
                        (cpp//compile-command (cpp-cmake//config-build)))))
      (if command
          (compile command)
        (let ((compile-command
               (or (-when-let (dir (cpp-cmake//locate-cmakelists nil nil "Makefile"))
                     (cpp//compile-command dir))
                   (ignore-errors (cpp-ccls//buffer-compile-command))
                   compile-command)))
          (call-interactively 'compile))))))

(defun cpp/gdb (&optional directory)
  (interactive
   (list (expand-file-name
          (read-directory-name
           "Directory: "
           (file-name-as-directory (or (and cpp-cmake-project-root
                                            (cpp-cmake//config-build))
                                       default-directory))
           ""
           :must-match))))
  (unless (featurep 'gud)
    (require 'gud nil :noerror))
  (let ((default-directory directory)
        (gud-chdir-before-run nil))
    (call-interactively #'gdb)))

(defun cpp/electric-star (-arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value -arg))
        (insert "  */")
        (backward-char 3)
        (indent-according-to-mode))
    (call-interactively 'self-insert-command)))

(defun cpp/begining-of-statment ()
  (interactive)
  (if (and (not current-prefix-arg)
           (or (= (char-before) ?\})
               (= (char-after) ?\})))
      (backward-sexp)
    (call-interactively 'c-beginning-of-statement)))

(defun cpp/end-of-statment ()
  (interactive)
  (if (and (not current-prefix-arg)
           (or (= (char-before) ?\{)
               (= (char-after) ?\{)))
      (forward-sexp)
    (call-interactively 'c-end-of-statement)))

(defun cpp/macro-expand ()
  (interactive)
  (setq-local c-macro-preprocessor
              (cpp-ccls//buffer-compile-command t))
  (call-interactively 'c-macro-expand))



(with-eval-after-load 'projectile
  (advice-add 'projectile--run-project-cmd
              :around
              (lambda (-fn &rest -args)
                (with-temp-env! (cpp-cmake//config-env) (apply -fn -args)))))

(with-eval-after-load 'cc-mode
  (require 'ccls)

  (dolist (key '("#" "}" "/" ";" "," ":" "(" ")" "{"))
    (define-key c-mode-base-map key nil))

  ;; Smart tab
  (advice-add 'c-indent-line-or-region :around #'core*indent-for-tab)

  (when (>= (string-to-number c-version) 5.33)
    (put 'c++-mode 'derived-mode-parent 'prog-mode)
    (put 'c-mode 'derived-mode-parent 'prog-mode)
    (put 'java-mode 'derived-mode-parent 'prog-mode))

  (define-key! :map c-mode-base-map
    ("*" . cpp/electric-star)
    ("M-a" . cpp/begining-of-statment)
    ("M-e" . cpp/end-of-statment)
    ("C-c o" . ff-find-other-file)
    ("C-c C-j" . semantic-ia-fast-jump)
    ("C-c C-v" . semantic-ia-show-variants)
    ("M-n" . next-error)
    ("M-p" . previous-error)
    ("C-M-i" . counsel-company))

  (define-key! :map c++-mode-map
    ("<")
    (">")
    ("C-c C-d")
    ("C-c C-e" . cpp/macro-expand)
    ("C-c b" . clang-format-buffer)
    ("C-c C-b" . clang-format-buffer)
    ("C-c C-l" . cpp/load-file-in-root)
    ("C-c T" . cpp-cmake/toggle-option)
    ("C-c C" . cpp-cmake/change-config)
    ("C-c D" . cpp-cmake/config)
    ("C-c C-c" . cpp/config-project)
    ("M-s l" . ccls-code-lens-mode)
    ("M-s c" . ccls-call-hierarchy)
    ("M-s m" . ccls-member-hierarchy)
    ("M-s i" . ccls-inheritance-hierarchy)
    ("C-c j" :map cpp-ccls-jump-map)
    ([f9] . cpp/config-project)
    ([f10] . cpp/compile)
    ([f5] . cpp/gdb)))

;; Set term default directory
(when (boundp 'term-default-directory-function-list)
  (add-to-list 'term-default-directory-function-list
               (lambda ()
                 (and cpp-cmake-project-root (cpp-cmake//config-build)))))

(when (boundp 'term-default-environment-function-list)
  (add-to-list 'term-default-environment-function-list 'cpp-cmake//config-env))

(cond ((eq font-lock-maximum-decoration t)
       (setq font-lock-maximum-decoration
             '((c++-mode . 1) (c-mode . 1) (t . t))))
      ((listp font-lock-maximum-decoration)
       (dolist (mode '(c++-mode c-mode))
         (let ((cell (assoc mode font-lock-maximum-decoration)))
           (if cell
               (setcdr cell 1)
             (push (cons mode 1) font-lock-maximum-decoration))))))


(provide 'init-cpp)
