(require-packages!
 lsp-javascript-typescript
 js-doc
 js2-mode)



(defun js2/print-json-path (&optional $hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is
replaced with it."
  (interactive "P")
  (let (previous-node
        (current-node (js2-node-at-point))
        result)
    ;; The `js2-node-at-point' starts scanning from AST root node.
    ;; So there is no way to optimize it.
    (while (not (js2-ast-root-p current-node))
      (cond
       ;; JSON property node
       ((js2-object-prop-node-p current-node)
        (push (concat "." (js2-prop-node-name
                           (js2-object-prop-node-left current-node)))
              result))
       ;; Array node
       ((or (js2-array-node-p current-node))
        (push (js2-get-element-index-from-array-node
               previous-node
               current-node
               $hardcoded-array-index)
              result))
       ;; Other nodes are ignored
       (t))
      ;; Current node is archived
      (setq previous-node current-node)
      ;; Get parent node and continue the loop
      (setq current-node (js2-node-parent current-node)))
    (if (equal (car result) ".")
        (setq result (cdr result)))
    (setq result (string-join result))
    (if result
        (progn
          (kill-new result)
          (message "%s => kill-ring" result))
      (message "No JSON path found!"))
    result))

(with-eval-after-load 'js2-mode
  (require 'lsp-javascript-typescript)
  (setq-default js2-use-font-lock-faces t
                js2-mode-must-byte-compile nil
                js2-idle-timer-delay 1
                js2-auto-indent-p nil
                js2-indent-on-enter-key nil
                js2-skip-preprocessor-directives t
                js2-strict-inconsistent-return-warning nil
                js2-enter-indents-newline nil
                js2-bounce-indent-p t)
  (setq-default js2-additional-externs
                '("$"
                  "angular" "app" "beforeEach" "browser"
                  "by" "clearInterval" "clearTimeout"
                  "inject" "it" "jQuery" "jasmine"
                  "module"
                  "process" "require" "setInterval" "setTimeout")))

(define-hook! js2|setup (js2-mode-hook)
  (js2-imenu-extras-mode)

  (unless (buffer-temporary?)
    (lsp-javascript-typescript-enable)
    (add-to-list 'company-backends 'company-lsp)))

(provide 'init-javascript)
