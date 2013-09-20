(defvar netlogo-mode-hook nil)

(defvar netlogo-mode-map
  (let ((netlogo-mode-map (make-keymap)))
    (define-key netlogo-mode-map "\C-j" 'newline-and-indent)
    netlogo-mode-map)
  "Keymap for NETLOGO major mode")

(defvar netlogo-mode-syntax-table
  (let ((netlogo-mode-syntax-table (make-syntax-table)))
    
    ;; Symbols
    (modify-syntax-entry ?_  "_"  netlogo-mode-syntax-table)

    ;; Operators (punctuation)
    (modify-syntax-entry ?+  "." netlogo-mode-syntax-table)
    ;(modify-syntax-entry ?-  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?*  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?/  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?%  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?&  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?|  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?^  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?!  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?=  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?<  "." netlogo-mode-syntax-table)
    (modify-syntax-entry ?>  "." netlogo-mode-syntax-table)

    (modify-syntax-entry ?- "w" netlogo-mode-syntax-table)
    (modify-syntax-entry ?( "w" netlogo-mode-syntax-table)
    (modify-syntax-entry ?) "w" netlogo-mode-syntax-table)
    (modify-syntax-entry ?\; "< b" netlogo-mode-syntax-table)
    (modify-syntax-entry ?[ "(" netlogo-mode-syntax-table)
    (modify-syntax-entry ?] ")" netlogo-mode-syntax-table)
    (modify-syntax-entry ?\n "> b" netlogo-mode-syntax-table)
    netlogo-mode-syntax-table)
  "Syntax table for netlogo-mode")

;; define several class of keywords
(defvar netlogo-logic-keywords
  '("and" "ask" "ask-concurrent" "carefully" "end" "error-message" "every"
    "foreach" "if" "ifelse" "ifelse-value" "let" "loop" "map" 
    "not" "or" "repeat" "report"  "run" "runresult" "set" "stop" 
    "startup" "wait" "while" "with-local-randomness"
    "without-interruption" "xor")
  "NETLOGO keywords.")

;;LOT OF KEYWORD
(defvar netlogo-keywords-regexp (regexp-opt netlogo-logic-keywords 'words))

(setq netlogo-keywords-breeds-left "own\\|at\\|here\\|on")
(setq netlogo-keywords-breeds-right "create\\|create-ordered\\|hatch\\|sprout\\|is\\|at\\|here\\|on")

;; create the list for font-lock.
;; each class of keyword is given a particular face
(setq netlogo-font-lock-keywords
      `(
	(,netlogo-keywords-regexp . font-lock-keyword-face)
	
	;; FUNCTION NAMES in declarations	
	("\\(\\<\\(?:to-report\\|to\\)\\>\\)\s*\\(\\w+\\)\s*\\(\\[\\(\\(\s*?\\<\\w+\\>\s*?\\)+\\)\\]\\)?"
	 (1 'font-lock-keyword-face)
	 (2 'font-lock-function-name-face)
	 (3 'font-lock-variable-name-face)
	 )
	
	;; ASK 
	("\\(\\<ask\\>\\)\s*\\(\\w+\\)\s*\\(with\s*\\[\\w+\\]\\)?"
;;	 (1 'font-lock-keyword-face)
;;	 (2 'font-lock-function-name-face)
;;	 (3 'font-lock-variable-name-face)
	 )

	;;METHOD PARAM definition
	;;FIXME : Ne marche pas encore ...
	(,(concat "\\(\\<\\w+-\\(?:" netlogo-keywords-breeds-left "\\)\\>\\)")
	 (1 'font-lock-keyword-face))
	(,(concat "\\(\\<\\(?:"netlogo-keywords-breeds-right"\\)-\\w+\\>\\)")
	 (1 'font-lock-keyword-face))
	;; note: order above matters. “mylsl-keywords-regexp” goes last because
	;; otherwise the keyword “state” in the function “state_entry”
	;; would be highlighted.
	
       ))

(defun netlogo-mode ()
  "Major mode for editing Workflow Process Description Language files"
  (interactive)
  (kill-all-local-variables)
  (set-syntax-table netlogo-mode-syntax-table)
  (use-local-map netlogo-mode-map)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((netlogo-font-lock-keywords)))

  ;;(set (make-local-variable 'font-lock-defaults) '(netlogo-font-lock-keywords))
  ;;(set (make-local-variable 'indent-line-function) 'netlogo-indent-line)  

  (setq major-mode 'netlogo-mode)
  (setq mode-name "NETLOGO")
  (run-hooks 'netlogo-mode-hook))

(message "netlogo-mode.el(c) is loaded")

(provide 'netlogo-mode)