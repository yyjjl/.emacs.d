;; -*- lexical-binding:t -*-

(cond ((eq font-lock-maximum-decoration t)
       (setq font-lock-maximum-decoration
             '((c++-mode . 1) (c-mode . 1) (t . t))))
      ((listp font-lock-maximum-decoration)
       (dolist (mode '(c++-mode c-mode))
         (setf (alist-get mode font-lock-maximum-decoration) 1))))

(eval-when-has-feature! lsp
  (after! lsp-mode
    (setq lsp-clients-clangd-executable ymacs-clangd-path)
    (setq lsp-clients-clangd-args
          '("--all-scopes-completion"
            "--clang-tidy"
            "--suggest-missing-includes")))

  (after! lsp-clangd
    (setf (lsp--client-download-server-fn (ht-get lsp-clients 'clangd))
          (ymacs-lsp//make-download-from-github-fn
           "clangd/clangd"
           (lambda (x) (string-match-p (if sys/linuxp "linux" "mac") x))
           :zip))))

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
      ("C-c C-b" . clang-format-buffer)
      ("C-c C-l" . ymacs-term/load-file-in-repl)))

  (dolist (key '("#" "}" "/" ";" "," ":" "(" ")" "{"))
    (define-key c-mode-base-map key nil)))
