;; -*- lexical-binding:t -*-

(executable! clangd :-exe [(expand-cache! "lsp/clangd/bin/clangd") "clangd"])

(require-packages! clang-format google-c-style)

(defvar-local ymacs-cpp-current-build-system nil)
(defvar ymacs-cpp-build-systems nil)

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

(after-feature! term
  ;; set term default directory
  (add-to-list 'ymacs-term-directory-functions #'ymacs-cpp//build-dir))

(eval-when-has-feature! lsp
  (ymacs-lsp//register-client
   'clangd
   :package 'lsp-clangd
   :manual `(:title "Clangd"
             :repo "clangd/clangd"
             :exe ,ymacs-clangd-path)))
