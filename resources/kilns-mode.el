(define-derived-mode kilns-mode lisp-mode "Kilns"
  "Major mode for the Kilns progamming language.
\\{kilns-mode-map}"
  (set (make-local-variable 'font-lock-defaults) '(kilns-font-lock-keywords))
  ;; TODO: modifying the syntax table might not be the best way to make these
  ;;       chars recognized as constituents.
  (modify-syntax-entry ?_ "w" kilns-mode-syntax-table)
  (modify-syntax-entry ?- "w" kilns-mode-syntax-table)
  (modify-syntax-entry ?* "w" kilns-mode-syntax-table))

(add-to-list 'auto-mode-alist '("\\.kiln\\'" . kilns-mode))

(setq kilns-font-lock-keywords
  `((";+" . font-lock-comment-delimiter)
    (,(regexp-opt '("trigger*" "list" "load" "sandbox") 'words) .
     font-lock-builtin-face)
    (,(regexp-opt '("new" "trigger" "up" "down" "par" "def") 'words) .
     font-lock-keyword-face)
    ;; FIXME: shouldn't be highlighting the "(", but can't do lookbehind
    ("(\\w+" . font-lock-function-name-face)
    ("?\\w+" . font-lock-variable-name-face)
    ("\\w+" . font-lock-constant-face)))

(define-key kilns-mode-map (kbd "C-c C-z") 'run-kilns)

(provide 'kilns-mode)
