;;; -*- lexical-binding: t; -*-

(defvar ymacs-debug--help-window nil)
(defvar ymacs-debug--help-buffer-name " *DEBUG-HELP*")

(defvar-local ymacs-debug--buffer-read-only nil)
(defvar-local ymacs-debug--dap-cookie nil)
(defvar-local ymacs-debug--buffer-position nil)

(defvar ymacs-debug--window-configuration nil)
(defvar ymacs-debug--buffers nil)
(defvar ymacs-debug-info-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    map))

(defvar ymacs-debug-running-session-mode-map
  (let ((map (make-sparse-keymap)))
    (suppress-keymap map)
    (define-key! :map map
      ("Q" . ymacs-debug/quit)

      ("u" . gud-up)
      ("d" . gud-down)
      ("j" . gud-jump)
      ("U" . gud-util)

      ("T" . gud-tbreak)
      ("b" . gud-break)
      ("D" . gud-remove)

      ("p" . gud-print)
      ("e" . gud-print)
      ("P" . gud-pstar)

      ("s" . gud-step)
      ("i" . gud-stepi)
      ("n" . gud-next)
      ("I" . gud-nexti)
      ("c" . gud-cont)

      ("f" . gud-finish)
      ("R" . gud-run)
      ("g" . gud-go)
      ("W" . gud-watch))))

(defvar ymacs-debug--help-format
  (eval-when-compile
    (let ((str (concat
                "Commands: "
                "_n_ext, next_I_, _s_tep, step_i_, _c_ontinue, "
                "_u_p, _d_own, _j_ump, _U_ntil, _b_reak, _D_elete, _T_break, "
                "_p_rint, _e_val, _P_star, _R_un, _f_inish, _g_o, _W_atch")))
      (while (string-match "_\\([^_]+?\\)_" str)
        (setq str (replace-match
                   (propertize (match-string 1 str) 'face 'font-lock-keyword-face)
                   t nil str)))
      str)))
(defvar ymacs-debug--gdb-help-format "")

(defmacro ymacs-debug//define-goto-command (name key)
  (let* ((parts (split-string (symbol-name name) "-"))
         (parts (if (equal (car (last parts)) "buffer")
                    (butlast parts)
                  parts))
         (function-name (string-join parts "-")))
    `(progn
       (define-key ymacs-debug-info-mode-map
         (kbd ,key)
         (defun ,(intern (format "ymacs-debug/display-%s" function-name)) ()
           (interactive)
           (let ((buffer (gdb-get-buffer-create ',name)))
             (pop-to-buffer buffer)
             (with-current-buffer buffer
               (if (eq ',name 'gdb-inferior-io)
                   (ymacs-debug-command-buffer-mode 1)
                 (ymacs-debug-info-buffer-mode 1))
               (tab-line-mode -1)))))

       (setq ymacs-debug--gdb-help-format
             (concat ymacs-debug--gdb-help-format
                     (propertize ,(format "[%s] " key)
                                 'face 'font-lock-keyword-face)
                     ,(string-join (cdr parts) "-")
                     " ")))))

(ymacs-debug//define-goto-command gdb-locals-buffer "ll")
(ymacs-debug//define-goto-command gdb-inferior-io "lo")
(ymacs-debug//define-goto-command gdb-stack-buffer "ls")
(ymacs-debug//define-goto-command gdb-breakpoints-buffer "lb")
(ymacs-debug//define-goto-command gdb-registers-buffer "lr")
(ymacs-debug//define-goto-command gdb-disassembly-buffer "ld")
(ymacs-debug//define-goto-command gdb-memory-buffer "lm")
(ymacs-debug//define-goto-command gdb-threads-buffer "lt")
