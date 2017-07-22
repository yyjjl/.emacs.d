(defun js2-print-json-path (&optional hardcoded-array-index)
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
               hardcoded-array-index)
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

(defhook js2|setup (js2-mode-hook)
  (js2-imenu-extras-mode)
  (setq mode-name "JavaScript2")
  (js2r-add-keybindings-with-prefix "C-c j")
  (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js)
  (js2-refactor-mode 1)
  (unless (buffer-temporary-p)

    (tern-mode 1)
    (add-to-list 'company-backends 'company-tern)))

;;  `flyspell' setup for js2-mode
(defun js2|flyspell-verify ()
  (let* ((f (get-text-property (- (point) 1) 'face)))
    ;; Only words with following font face will be checked
    (memq f '(js2-function-call
              js2-function-param
              js2-object-property
              font-lock-variable-name-face
              font-lock-string-face
              font-lock-function-name-face
              font-lock-builtin-face
              rjsx-tag
              rjsx-attr))))
(put 'js2-mode 'flyspell-mode-predicate 'js2|flyspell-verify)

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
