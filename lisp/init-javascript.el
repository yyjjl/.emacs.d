(defun js2-print-json-path (&optional hardcoded-array-index)
  "Print the path to the JSON value under point, and save it in the kill ring.
If HARDCODED-ARRAY-INDEX provided, array index in JSON path is replaced with it."
  (interactive "P")
  (let (previous-node current-node
                      key-name
                      rlt)

    ;; The `js2-node-at-point' starts scanning from AST root node.
    ;; So there is no way to optimize it.
    (setq current-node (js2-node-at-point))

    (while (not (js2-ast-root-p current-node))
      (cond
       ;; JSON property node
       ((js2-object-prop-node-p current-node)
        (setq key-name (js2-prop-node-name
                        (js2-object-prop-node-left current-node)))
        (if rlt (setq rlt (concat "." key-name rlt))
          (setq rlt (concat "." key-name))))

       ;; Array node
       ((or (js2-array-node-p current-node))
        (setq rlt (concat
                   (js2-get-element-index-from-array-node
                    previous-node
                    current-node
                    hardcoded-array-index)
                   rlt)))
       ;; Other nodes are ignored
       (t))
      ;; current node is archived
      (setq previous-node current-node)
      ;; Get parent node and continue the loop
      (setq current-node (js2-node-parent current-node)))
    (cond
     (rlt
      ;; Clean the final result
      (setq rlt (replace-regexp-in-string "^\\." "" rlt))
      (kill-new rlt)
      (message "%s => kill-ring" rlt))
     (t
      (message "No JSON path found!")))
    rlt))

(defun my-js2-mode-setup()
    (unless (is-buffer-file-temp)
      (js2-imenu-extras-mode)
      (setq mode-name "JS2")

      (js2-refactor-mode 1)

      (tern-mode 1)

      (add-to-list 'company-backends 'company-tern)

      (js2r-add-keybindings-with-prefix "C-c j")
      (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js)))

(add-hook 'js2-mode-hook 'my-js2-mode-setup)

(setq-default js2-use-font-lock-faces t
              js2-mode-must-byte-compile nil
              js2-idle-timer-delay 0.5 ; NOT too big for real time syntax check
              js2-auto-indent-p nil
              js2-indent-on-enter-key nil ; annoying instead useful
              js2-skip-preprocessor-directives t
              js2-strict-inconsistent-return-warning nil ; return <=> return null
              js2-enter-indents-newline nil
              js2-bounce-indent-p t)
(setq-default js2-additional-externs
              '("$"
                "$A" ; salesforce lightning component
                "AccessifyHTML5" "KeyEvent"
                "Raphael" "React" "_content" ; Keysnail
                "angular" "app" "beforeEach" "browser"
                "by" "clearInterval" "clearTimeout"
                "command" ; Keysnail
                "content" ; Keysnail
                "define" "describe"
                "display" ; Keysnail
                "element" "expect"
                "ext" ; Keysnail
                "gBrowser" ; Keysnail
                "goDoCommand" ; Keysnail
                "hook" ; Keysnail
                "inject" "it" "jQuery" "jasmine"
                "key" ; Keysnail
                "ko" "log" "module"
                "plugins" ; Keysnail
                "process" "require" "setInterval" "setTimeout"
                "shell" ; Keysnail
                "tileTabs" ; Firefox addon
                "util" ; Keysnail
                "utag"))

(provide 'init-javascript)
