(defvar! js2-has-tern-p (executable-find "tern")
  "Js context sensitive completion")
(defvar! js2-has-web-beautify-p (executable-find "js-beautify")
  "Like clang-format")

(require! 'js-doc)
(require! 'js2-mode)
(require! 'js-comint)
(require! 'js2-refactor)
(when js2-has-web-beautify-p
  (require! 'web-beautify))
(when js2-has-tern-p
  (require! 'company-tern)
  (require! 'tern))



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
  (when js2-has-web-beautify-p
    (define-key js2-mode-map (kbd "C-c b") 'web-beautify-js)))

(define-hook! js2|setup (js2-mode-hook)
  (js2-imenu-extras-mode)
  (setq mode-name "Js2")
  (js2r-add-keybindings-with-prefix "C-c j")
  (js2-refactor-mode 1)

  (unless (and (buffer-temporary?)
               js2-has-tern-p)
    (tern-mode 1)
    (add-to-list 'company-backends 'company-tern)))

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
