;;; -*- lexical-binding: t; -*-

(eval-when-compile
  (require 'eglot))

(defvar ymacs-lsp-signature-doc-lines 6)

(defvar-local ymacs-lsp--signature-last nil)
(defvar-local ymacs-lsp--signature-last-index nil)
(defvar ymacs-lsp--signature-last-buffer nil)

(defun ymacs-lsp/signature-stop ()
  "Stop showing current signature help."
  (interactive)
  (remove-hook 'post-command-hook #'ymacs-lsp//signature)
  (lv-delete-window)
  (eglot--when-live-buffer ymacs-lsp--signature-last-buffer
    (ymacs-lsp-signature-mode -1))
  (setq ymacs-lsp--signature-last-buffer nil))

(defun ymacs-lsp/signature-start ()
  "Activate signature help.
It will show up only if current point has signature help."
  (interactive)
  (setq ymacs-lsp--signature-last nil
        ymacs-lsp--signature-last-index nil
        ymacs-lsp--signature-last-buffer (current-buffer))
  (add-hook 'post-command-hook #'ymacs-lsp//signature)
  (ymacs-lsp-signature-mode 1))

(defun ymacs-lsp//signature-display (-message)
  (when -message
    (let ((lv-force-update t))
      (lv-message "%s" -message))))

(defun ymacs-lsp//signature ()
  "Display signature info (based on `textDocument/signatureHelp')"
  (if (and ymacs-lsp--signature-last-buffer
           (not (equal (current-buffer) ymacs-lsp--signature-last-buffer)))
      (ymacs-lsp/signature-stop)
    (when (eglot-server-capable :signatureHelpProvider)
      (let ((buf (current-buffer)))
        (jsonrpc-async-request
         (eglot--current-server-or-lose)
         :textDocument/signatureHelp (eglot--TextDocumentPositionParams)
         :success-fn
         (eglot--lambda ((SignatureHelp)
                         signatures activeSignature (activeParameter 0))
           (eglot--when-buffer-window buf
             (ymacs-lsp//signature-display (ymacs-lsp//signature->message (list signatures activeSignature activeParameter)))))
         :deferred :textDocument/signatureHelp))
      t)))

(defun ymacs-lsp/signature-next ()
  "Show next signature."
  (interactive)
  (let ((nsigs (length (car-safe ymacs-lsp--signature-last))))
    (when (and ymacs-lsp--signature-last-index
               ymacs-lsp--signature-last)
      (setq ymacs-lsp--signature-last-index (% (1+ ymacs-lsp--signature-last-index) nsigs))
      (ymacs-lsp//signature-display (ymacs-lsp//signature->message ymacs-lsp--signature-last)))))

(defun ymacs-lsp/signature-previous ()
  "Next signature."
  (interactive)
  (let ((nsigs (length (car-safe ymacs-lsp--signature-last))))
    (when (and ymacs-lsp--signature-last-index
               ymacs-lsp--signature-last)
      (setq ymacs-lsp--signature-last-index (1- (if (zerop ymacs-lsp--signature-last-index)
                                                    nsigs
                                                  ymacs-lsp--signature-last-index)))
      (ymacs-lsp//signature-display (ymacs-lsp//signature->message ymacs-lsp--signature-last)))))

(defun ymacs-lsp/signature-toggle-full-docs ()
  "Toggle full/partial signature documentation."
  (interactive)
  (setq ymacs-lsp-signature-doc-lines (if (numberp ymacs-lsp-signature-doc-lines)
                                          (list ymacs-lsp-signature-doc-lines)
                                        (or (car-safe ymacs-lsp-signature-doc-lines) 6)))
  (ymacs-lsp/signature-start))

(defun ymacs-lsp//signature->message (-signature-help)
  (setq ymacs-lsp--signature-last -signature-help)

  (-when-let* (((signatures active-signature-index active-parameter-index) -signature-help)
               (num-signatures (length signatures))
               (active-signature-index (or ymacs-lsp--signature-last-index active-signature-index 0))
               (signature (when (> num-signatures 0)
                            (aref signatures active-signature-index))))
    (setq ymacs-lsp--signature-last-index active-signature-index)

    (when-let* ((prefix (propertize (format "[%s/%s]" (1+ active-signature-index) num-signatures) 'face 'highlight))
                (message (eglot--sig-info signature active-parameter-index))
                (message (concat prefix " " message))
                (lines (save-match-data (split-string message "\\(\r\n\\|[\n\r]\\)"))))
      (when (and (numberp ymacs-lsp-signature-doc-lines)
                 (> (length lines) ymacs-lsp-signature-doc-lines))
        (setq message
              (concat (string-join (seq-take lines ymacs-lsp-signature-doc-lines) "\n")
                      "\n"
                      (propertize "Truncated..." 'face 'highlight))))
      message)))

(defvar ymacs-lsp-signature-mode-map
  (define-key! :map (make-sparse-keymap)
    ("M-n" . ymacs-lsp/signature-next)
    ("M-p" . ymacs-lsp/signature-previous)
    ("M-a" . ymacs-lsp/signature-toggle-full-docs)
    ("M-o" . ymacs-lsp/signature-stop)
    ("C-g" . ymacs-lsp/signature-stop))
  "Keymap for `ymacs-lsp-signature-mode'.")

(define-minor-mode ymacs-lsp-signature-mode
  "Mode used to show signature popup."
  :keymap ymacs-lsp-signature-mode-map
  :lighter ""
  :group 'eglot
  (if ymacs-lsp-signature-mode
      (remove-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function t)
    (add-hook 'eldoc-documentation-functions #'eglot-signature-eldoc-function nil t)))
