;; -*- lexical-binding:t -*-

(ymacs-editor//set-forward-sexp-handler
 :modes (c-mode c++mode java-mode)
 :forward #'c-end-of-statement)

(ymacs-editor//set-font-lock-level
  :modes (c-mode c++-mode)
  :level 1
  :keywords
  (eval-when-compile
    `((,(rx symbol-start
            (or "alignas" "alignof" "and" "and_eq" "asm"
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
                "volatile" "while" "xor" "xor_eq")
            symbol-end)
       . font-lock-keyword-face)
      (,(rx symbol-start
            (or "bool" "float" "double" "int" "long"
                "char" "char16_t" "char32_t" "wchar_t" "unsigned")
            symbol-end)
       . font-lock-type-face)
      (,(rx symbol-start (or "NULL" "nullptr" "false" "true") symbol-end)
       . font-lock-constant-face))))

(eval-when-has-feature! lsp
  (after! (lsp-mode lsp-clangd)
    (dolist (arg '("--all-scopes-completion"
                   "--background-index"
                   "--cross-file-rename"
                   "--clang-tidy"))
      (cl-pushnew arg lsp-clients-clangd-args :test #'string=))))

(after! cc-mode
  (ymacs-editor//add-toggles
   "C/C++"
   '(memq major-mode '(c-mode c++-mode))
   '("c" c-toggle-comment-style "comment-style")
   '("e" c-toggle-electric-state "electric" :toggle c-electric-flag)
   '("a" c-toggle-auto-newline "auto \\n" :toggle c-auto-newline)
   '("h" c-toggle-hungry-state "hungry delete" :toggle c-hungry-delete-key)

   '("d" ymacs-cpp-cmake/config :exit t)
   '("t" ymacs-cpp-cmake/toggle-option :exit t)
   '("C" ymacs-cpp-cmake/change-config :exit t)
   '("L" ymacs-cpp/load-in-repl :exit t))

  (dolist (map (list c-mode-map c++-mode-map))
    (define-key! :map map
      (("<" ">" "C-c C-d"))             ; unbind
      ("*" . ymacs-cpp/electric-star)
      ("C-c C-l" . ymacs-term/load-file-in-repl)))

  (dolist (key '("#" "}" "/" ";" "," ":" "(" ")" "{"))
    (define-key c-mode-base-map key nil)))
