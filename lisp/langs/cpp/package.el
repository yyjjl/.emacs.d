;; -*- lexical-binding:t -*-

(option! cpp-clangd-version "11.0.0"
  "clangd version"
  :type 'string)

(executable! clangd :exe [(expand-cache! (format "lsp/clangd_%s/bin/clangd" ymacs-cpp-clangd-version)) "clangd"])

(require-packages! google-c-style)

(cl-defstruct ymacs-cpp-build-system
  (system-id nil)

  (lsp-enable-fn nil)
  (lsp-enable-handler nil)
  (lsp-disable-handler nil)

  (directory-fn nil)
  (command-fn nil))

(defconst ymacs-cpp-font-lock-keywords
  (eval-when-compile
    `((,(concat
         "\\<"
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
      (,(concat
         "\\<"
         (regexp-opt '("bool" "float" "double" "int" "long"
                       "char" "char16_t" "char32_t" "wchar_t"
                       "unsigned"))
         "\\>")
       . font-lock-type-face)
      (,(concat "\\<" (regexp-opt '("NULL" "nullptr" "false" "true")) "\\>")
       . font-lock-constant-face))))

(defvar-local ymacs-cpp-current-build-system nil)
(defvar ymacs-cpp-build-systems
  (list
   (make-ymacs-cpp-build-system
    :system-id 'default
    :lsp-enable-fn #'ymacs-cpp//get-dot-clangd-path
    :command-fn #'ymacs-cpp//get-compile-command-from-dot-clangd)))

(eval-when-has-feature! term
  ;; set term default directory
  (add-to-list 'ymacs-term-directory-functions #'ymacs-cpp//build-dir)
  (add-to-list 'ymacs-term-repl-alist
               '(c++-mode :program "root" :program-args ("-l" the-file) :cmd-fmt ".X %s\n")))

(eval-when-has-feature! debug
  (add-to-list 'ymacs-debugger-alist '(c-mode gdb :gud t))
  (add-to-list 'ymacs-debugger-alist '(c++-mode gdb :gud t)))

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client 'clangd :package 'lsp-clangd))
