;; -*- lexical-binding:t -*-

(setvar!
 cpp-cquery-base-path (expand-var! "cquery")
 cpp-cquery-path (expand-file-name "build/cquery"
                                   cpp-cquery-base-path)
 cpp-has-cmake-p (executable-find "cmake")
 cpp-has-cquery-p (file-exists-p cpp-cquery-base-path))

(require-packages!
 (ggtags :when env-has-gtags-p)
 (lsp-mode :when cpp-has-cquery-p)
 (company-lsp :when cpp-has-cquery-p)
 (cquery :when cpp-has-cquery-p)
 clang-format
 google-c-style)

(require 'init-cpp-cquery)

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




(defun cpp//compile-command ($dir)
  "Return suitable compile command for current project"
  (cond
   ((bound-and-true-p projectile-project-compilation-cmd)
    projectile-project-compilation-cmd)
   ((file-exists-p (expand-file-name "build.ninja" $dir))
    (concat "ninja -C " $dir))
   ((file-exists-p (expand-file-name "Makefile" $dir))
    (concat "make --no-print-directory -C " $dir))
   (t nil)))



;; Setup

(defun cpp//common-cc-setup ()
  "Setup shared by all languages (java/groovy/c++ ...)"
  (highlight-indentation-mode 1)
  (turn-on-auto-fill)
  (google-set-c-style)
  (setq c-basic-offset 4)
  ;; make DEL take all previous whitespace with it
  ;; (c-toggle-auto-newline 1)
  (highlight-indentation-set-offset c-basic-offset)
  (c-toggle-hungry-state 1))

(defun cpp//font-lock-setup ()
  (when (or (eq font-lock-maximum-decoration 1)
            (and (listp font-lock-maximum-decoration)
                 (eq (cdr (assoc major-mode
                                 font-lock-maximum-decoration))
                     1)))
    (font-lock-add-keywords nil cpp--font-lock-keywords)))

(defun cpp//setup ()
  (if (or (not cpp-has-cmake-p)
          (not cpp-has-cquery-p)
          (file-remote-p default-directory)
          (bound-and-true-p cpp-setup-literally)
          (> (buffer-size) core-large-buffer-size))
      (progn
        (ggtags-mode 1)
        ;; (setq completion-at-point-functions nil)
        (flycheck-mode -1))
    (setq cpp-cmake-project-root (cpp-cmake//locate-cmakelists))
    (if (or (file-exists-p (cpp-cquery//dot-cquery-path))
            (and cpp-cmake-project-root (file-exists-p (cpp-cmake//cdb-path))))
        (cpp-cquery//setup)
      (when cpp-cmake-project-root
        (message "Need run `cpp/run-cmake' to setup cquery server")))))

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

      (unless (buffer-temporary?)
        (add-transient-hook!
            (hack-local-variables-hook :local t :name cpp//setup-interal)
          (cpp//setup))))))



(defun cpp/run-cmake (&optional $set-options)
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
           (cpp-cquery//setup)))))
    (when $set-options
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
           (buffer-name (format "*root:%s*" (buffer-name)))
           (buffer (get-buffer-create buffer-name))
           (proc (get-buffer-process buffer)))
      (if (and proc
               (process-live-p proc)
               (eq (buffer-local-value 'major-mode buffer)
                   'term-mode))
          (with-current-buffer buffer
            (term-send-raw-string (format ".X %s\n" file)))
        (kill-buffer buffer)
        (setq buffer (term/exec-program "root"
                                        (list "-l" (or file ""))
                                        buffer-name)))
      (term//pop-to-buffer buffer)))))

(defun cpp/compile ()
  (interactive)
  (let ((command (and cpp-cmake-project-root
                      (cpp//compile-command (cpp-cmake//config-build)))))
    (if command
        (with-temp-env! (cpp-cmake//config-env)
          (compile command))
      (setq command
            (or (-when-let (dir (cpp-cmake//locate-cmakelists
                                 nil nil "Makefile"))
                  (cpp//compile-command dir))
                (-when-let (info (ignore-errors (cquery-file-info)))
                  (concat (string-join (mapcar #'shell-quote-argument
                                               (gethash "args" info))
                                       " ")
                          " -o "
                          (ignore-errors
                            (file-name-base (buffer-file-name)))))))
      (let ((compile-command (or command compile-command)))
        (with-temp-env! (cpp-cmake//config-env)
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
  (let ((default-directory directory))
    (call-interactively #'gdb)))

(defun cpp/electric-star ($arg)
  (interactive "*P")
  (if (eq (char-before) ?\/)
      (progn
        (self-insert-command (prefix-numeric-value $arg))
        (insert "*/")
        (backward-char 2)
        (indent-according-to-mode))
    (call-interactively 'c-electric-star)))



(with-eval-after-load 'cc-mode
  (require 'cquery)

  ;; Smart tab
  (advice-add 'c-indent-line-or-region :around #'core*indent-for-tab)

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
    ("M-p" . flycheck-previous-error)
    ("C-M-i" . counsel-company))

  (define-key! :map c++-mode-map
    ("C-c C-d")
    ("C-c b" . clang-format-buffer)
    ("C-c C-b" . clang-format-buffer)
    ("C-c C-l" . cpp/load-file-in-root)
    ("C-c T" . cpp-cmake/toggle-option)
    ("C-c D" . cpp-cmake/config)
    ("C-c C-c" . cpp/run-cmake)
    ("M-s l" . cquery-code-lens-mode)
    ("M-s c" . cquery-call-hierarchy)
    ("M-s m" . cquery-member-hierarchy)
    ("M-s i" . cquery-inheritance-hierarchy)
    ("C-c j" :map cpp-cquery-jump-map)
    ([f9] . cpp/run-cmake)
    ([f10] . cpp/compile)
    ([f5] . cpp/gdb)))

;; Set term default directory
(when (boundp 'term-default-directory-function-list)
  (add-to-list 'term-default-directory-function-list
               (byte-compile
                (lambda ()
                  (and cpp-cmake-project-root (cpp-cmake//config-build))))))

(when (boundp 'term-default-environment-function-list)
  (add-to-list 'term-default-environment-function-list
               (byte-compile
                (lambda ()
                  (and cpp-cmake-project-root (cpp-cmake//config-env))))))

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
