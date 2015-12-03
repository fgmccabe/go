;;; 
;;; Go! Emacs mode
;;; Copyright (C) 2000 F.G. McCabe

;;; Originally based on april mode authored by J. Knottenbelt
;;; Made to work by F.G.McCabe <fgm@fla.fujitsu.com>
;;; Modifications J Knottenbelt <jak@fla.fujitsu.com>

;;; Keywords: languages

;;; GNU Emacs is free software; you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation; either version 2, or (at your option)
;;; any later version.

;;; GNU Emacs is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.

;;; You should have received a copy of the GNU General Public License
;;; along with GNU Emacs; see the file COPYING.  If not, write to the
;;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;;; Boston, MA 02111-1307, USA.

;;; Commentary

(require 'cl)
(require 'font-lock)
(require 'godebug )

(defvar
  go-xemacs (not (not (string-match "XEmacs" (emacs-version))))
  "Whether go-mode is running under XEmacs or not")

;; Customization parameters

(defgroup go nil
  "Major mode for editing and running Go under Emacs"
  :group 'languages)

(defcustom go-block-indent 2
  "* Amount by which to indent blocks of code in Go mode"
  :type 'integer
  :group 'go)

(defcustom go-paren-indent 1
  "* Amount by which to indent after a left paren in Go mode"
  :type 'integer
  :group 'go)

(defcustom go-brace-indent 2
  "* Amount by which to indent after a left brace in Go mode"
  :type 'integer
  :group 'go)

(defcustom go-bracket-indent 5
  "* Amount by which to indent after a left bracket in Go mode"
  :type 'integer
  :group 'go)

(defcustom go-arrow-indent 4
  "* Amount by which to indent after an arrow in Go mode"
  :type 'integer
  :group 'go)

(defcustom go-query-indent 2
  "* Amount by which to indent after an query in Go mode"
  :type 'integer
  :group 'go)

(defcustom comment-column 40
  "* The column where -- comments are placed"
  :type 'integer
  :group 'go)

;;; Initialise the syntax table

(defun go-modify-syntax (table &rest pairs)
  (while pairs
    (modify-syntax-entry (car pairs) (cadr pairs) table)
    (setq pairs (cddr pairs))))

(defvar go-mode-syntax-table nil
  "Syntax table used while in Go mode.")

(defvar go-debugging nil
  "Non-nil if should log messages to *go-debug*")

(if go-mode-syntax-table 
    nil
  (setq go-mode-syntax-table (make-syntax-table))
  ;; Comments
  (go-modify-syntax go-mode-syntax-table
		    ?/   ". 14"
		    ?*   ". 23"
		    ?\n  (if go-xemacs ">78b" ">56b"))
  ;; Symbols
  (go-modify-syntax go-mode-syntax-table
		    ?_   "w"
		    ?+   "_"
		    ?=   "_"
		    ?<   "_"
		    ?>   "_"
		    ?=   "_"
		    ?~   "_"
		    ?-   "_"
		    ?$   "_"
		    ?&   "_"
		    ?|   "."
		    ?\'  "\""
		    ?\"  "\""
;;		    ?\`  "\\"
		    ?%   "_"
		    ?^   "_"
		    ?\;   "."
		    ?    "    "
		    ?\t  "    "))

;;; Initialise the abbrev table
(defvar go-mode-abbrev-table nil
  "Abbrev table used while in Go mode.")
(define-abbrev-table 'go-mode-abbrev-table ())

;;; Initialise the key map
(defvar go-mode-map nil)
(if go-mode-map 
    nil
  (setq go-mode-map (make-sparse-keymap))
  (define-key go-mode-map "\t" 'indent-for-tab-command)
  (define-key go-mode-map [(control meta q)] 'go-indent-sexp)
  (define-key go-mode-map [(control c) (control c)] 'comment-region)
  (define-key go-mode-map [(control c) (control d)] 'godebug-buffer)
  (mapcar '(lambda (key-seq)
	     (define-key go-mode-map 
	       key-seq 
	       'go-self-insert-and-indent-command))
	  '("{" "}" ";" "|" "," "(" ")")))

(defun go-self-insert-and-indent-command (n)
  "Self insert and indent appropriately"
  (interactive "*P")
  (self-insert-command (prefix-numeric-value n))
  (indent-for-tab-command))

(defvar go-indent-cache nil
  "Incremental parse state cache")

;;; Provide `go-mode' user callable function
(defun go-mode ()
  "Major mode for editing Go programs"
  (interactive)
  (kill-all-local-variables)

  (use-local-map go-mode-map)
  (setq mode-name "Go!")
  (setq major-mode 'go-mode)

  (setq local-abbrev-table go-mode-abbrev-table)
  (set-syntax-table go-mode-syntax-table)

  (make-local-variable 'comment-start)
  (setq comment-start "-- ")

  (make-local-variable 'comment-end)
  (setq comment-end "")

  (make-local-variable 'parse-sexp-ignore-comments)
  (setq parse-sexp-ignore-comments t)

  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "^-- \\|[^:]-- ")

  ;; Local variables (indentation)
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'go-indent-line)

  ;; very important that case-fold-search is nil
  ;; since go! is a case-sensitive language
  (setq case-fold-search nil)

  ;; go-indent-cache holds the parse state 
  ;; at particular points in the buffer.
  ;; It is a sorted list (largest points first)
  ;; of elements (POINT . PARSE-STATE)
  ;; PARSE-STATE are cells (STATE . STACK)
  (make-local-variable 'go-indent-cache)
  (setq go-indent-cache nil)

  ;; After a buffer change, we need
  ;; to ensure that the cache is consistent.
  (make-local-variable 'before-change-functions)
  (add-to-list 'before-change-functions 'go-before-change-function)

  ;; Initialise font-lock support

  (go-init-font-lock)
  (run-hooks 'go-mode-hook))

(defun go-before-change-function (from to &rest rest)
  ;; The buffer has changed, we need to
  ;; remove any parse states that have been saved
  ;; past point 'from' in the buffer.
  (while (and go-indent-cache
	      (>= (caar go-indent-cache) from))
    (setq go-indent-cache (cdr go-indent-cache))))

;;; Indentation and syntax
(defsubst go-skip-block-comment ()
  (forward-comment 1))

(defsubst go-skip-line-comment ()
  (search-forward "\n"))

(defsubst go-skip-string ()
  (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))

(defsubst go-skip-symbol ()
  (goto-char (or (scan-sexps (point) 1) (buffer-end 1))))

(defsubst go-skip-char ()
  (cond ((looking-at "`\\+[0-9a-fA-F]+;")
	 (goto-char (match-end 0)))
	((looking-at "`\\\\")
	 (forward-char 2))
	(t (forward-char 2))))

(defun go-calculate-outer-indent (pos)
  (save-excursion
    (condition-case nil
	(progn (goto-char pos)
	       (goto-char (scan-lists pos -1 1))
	       (go-calculate-indent (point)))
      (error 0))))

;;; look for a the first non-whitespace
(defun go-indentation-level (pos)
  "returns the indentation level of the current line"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (current-column)))

(defun go-line-get-pos-after (pos what)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at what)
	(match-end 0)
      nil)))

(defun go-one-of (&rest l)
  (if (cadr l) 
      (concat (car l) "\\|"
	      (apply 'go-one-of (cdr l)))
    (car l)))

(defvar go-close-par "[])}]"
  "Go close parentheses")

(defvar go-line-comment "-- "
  "Go line comment")

(defvar go-body-comment "/\\*"
  "Go body comment")

(defvar go-comment (concat "\\(" 
			   (go-one-of go-line-comment go-body-comment)
			   "\\)")
  "Go comment")

(defvar go-comment-bol (concat "^" go-comment)
  "Go comment at beginning of line")

(defun go-up-level (pos)
  (or (condition-case nil (scan-lists pos -1 1) (error nil)) 
      0))

;;; Parse tables
(defconst go-operators
  ;; Prec Text  Regex  Push Pop Hanging IndentOption      Delta
  '((5000 "{"   "{"    t    nil  nil    nil               go-brace-indent)
    (5000 "["   "\\["  t    nil  t	nil		  go-bracket-indent)
    (5000 "("   "("    t    nil  t	nil		  go-paren-indent)
    (5000 ")"   ")"    nil  same nil	nil		  0)
    (5000 "]"   "\\]"  nil  same nil	nil		  0)
    (5000 "}"   "}"    nil  same nil	nil		  0)
    (1200 ":--" ":--"  t    t    nil	nil		  go-arrow-indent)
    (1200 ":-"  ":-"   t    t    t	nil		  go-arrow-indent)
    (1200 "-->"  "-->" t    t    nil	nil		  go-arrow-indent)
    (1100 "onerror"  
          "onerror\\b" t    t	 nil    nil               0)
    (900  ":="  ":="   t    t    nil    nil		  go-arrow-indent)
    (820  ".."  "\\.\\." t  t    nil	nil		  0)
    (1460 "::=" "::="  t    t    t	nil		  (* go-arrow-indent 2))
    (1200 "->"  "->"   t    t    nil	nil		  go-arrow-indent)
    (1199 "=>"  "=>"   t    t    t	nil		  go-arrow-indent)
    (1199 "<="  "<="   t    t    nil	nil		  go-arrow-indent)
    (1199 "<~"  "<~"   t    t    nil	nil		  go-arrow-indent)
    (1250 "|"   "|"    t    t    nil	nil		  0)
    (1060 "||"  "||"   t    t    nil	nil		  0)
    (1010  "::"  "::"   t    t    nil	nil		  (* go-arrow-indent 2))
    (1000 ","   ","    t    t    nil	nil		  0)
    (1000 ";"   ";"    t    t    nil	nil		  0)
    (900  ".="  "\\.=" t    t    nil    nil		  0)
    ;; "Terminating dot must be followed by whitespace"
    (1900 ". "  "\\. " t    t    nil    nil               0)
    (1900 ".\t"  "\\.\t" t  t    nil	nil		  0)
    (1900 ".\n"  "\\.\n" t  t    nil	nil		  0)
    (1040 "?"   "\\?"  t    t    nil	nil		  go-query-indent)
    (750  "private"  
          "private\\b" t    t    nil    nil		  0)
    (750  "import"  
          "import\\b" t    t    nil    nil		  0)
    (750  "action" 
	  "action\\b"  t    t    nil    nil		  0)
    (750  "sync" 
	  "sync\\b"  t    t    nil    nil		  0)
    )
  "Go operators and precedences")

;;; Speed up table lookups by encoding
;;; information in operator symbols.
(defun go-setup-operators-hash ()
  (let ((l go-operators))
    (while l 
      (let* ((o (car l))
	     (precedence (first o))
	     (text (second o))
	     (regex (third o))
	     (push (fourth o))
	     (pop  (fifth o))
	     (hanging (sixth o))
	     (option (seventh o))
	     (delta (eighth o))
	     (symbol (intern text)))
	(put symbol 'precedence precedence)
	(put symbol 'text text)
	(put symbol 'regex regex)
	(put symbol 'push push)
	(put symbol 'pop (if (eq pop 'same) nil pop))
	(put symbol 'pop-until-same (eq pop 'same))
	(put symbol 'hanging hanging)
	(put symbol 'delta delta)
	(put symbol 'length (length text)))
      (setq l (cdr l)))))
(go-setup-operators-hash)

;;; Regular expression matching important go operators
(defconst go-operators-regex
  (apply 'go-one-of
	 (mapcar 'caddr go-operators))
  "Regular expression matching important go operators")

(defconst go-escaped-string-regex "\\\\['\"]"
  "Regular expression matching the start of an escape")

(defconst go-next-token-regex
  (go-one-of go-operators-regex 
	     go-escaped-string-regex
	     "\""
	     "\'"
	     go-body-comment
	     go-line-comment))

;; The PARSE-STATE is a stack with at least one element.
;; Each element is a list with format (PRECEDENCE OP INDENT)
;; PREC: operator precedence
;; OP: symbol corresponsing to that operator
;; INDENT: indent calculated so far.
(defsubst go-init-parse-state ()
  (list 
   (list 9999 'none 0 nil)))

;; Accessor functions for a PARSE-STATE ((PREC OP INDENT) . STACK)
(defsubst go-parse-state-indent (parse-state)
  (third (car parse-state)))

(defsubst go-parse-state-op (parse-state)
  (second (car parse-state)))

(defsubst go-parse-state-in-comment (parse-state)
  (fourth (car parse-state)))

(defun go-parse-until (pos)
  ;; Find the most recent parse state in the cache 
  ;; that is <= pos
  (let ((parse-state (go-init-parse-state)) ; The parse-state just before POS
	(parse-pos   1)			; The position of the above parse-state
	(before      go-indent-cache)   ; All the parse-states before POS
	(after       nil))		; All the parse-states after POS
    (while (and before
		(> (caar before) pos))
      (setq after (cons (car before) after)
	    before (cdr before)))
    ;; Load the parse state
    (if before
	(setq parse-pos (caar before)
	      parse-state (cdar before)
	      before (cdr before)))
    (cond 
     ;; Have we already parsed up till here?
     ((= parse-pos pos)		
      parse-state)
     ;; Nope
     (t 
      ;; if there is an error parsing (eg. due to end-of-buffer)
      ;; just return 0
      (condition-case nil
	  (let ((new-parse-state (go-parse parse-pos pos parse-state)))
	    ;; If we parsed into a comment
	    ;; don't bother saving the parse state.
	    (if (go-parse-state-in-comment new-parse-state)
		new-parse-state
	      (progn
		;; Insert the new-parse-state into the indent-cache
		;; Cache is sorted, largest first.
		;; cache = (reverse after) <> [new-parse-state,parse-state,..before]	
		(setq go-indent-cache
		      (cons (cons parse-pos parse-state) 
			    before))
		(setq go-indent-cache
		      (cons (cons pos new-parse-state)
			    go-indent-cache))
		(while after
		  (setq go-indent-cache (cons (car after) go-indent-cache)
			after (cdr after)))
		new-parse-state)))
	(t ;; Some error occurred
	 parse-state)))
     )))

(defsubst go-calculate-brace-indent (pos)
  (go-parse-state-indent (go-parse-until pos)))

;;; Parse from POS to TO given initial PARSE-STATE
;;; Return final PARSE-STATE at TO.
(defun go-parse (pos to parse-state)
  (let* ((case-fold-search nil)
	 (state (car parse-state))
	 (stack (cdr parse-state))
	 (tos-prec   (first  state))
	 (tos-op     (second state))
	 (tos-indent (third  state))
	 (tos-in-comment (fourth state)))
    (save-excursion
      (goto-char pos)
      ;; We assume that the parsing does not
      ;; resume from within a (block) comment.
      ;; To implement that we would need
      ;; to check tos-in-comment and scan for
      ;; end-of-comment (*/) to escape it first.
      (progn 
	(while (< (point) to)
	  (cond 
	   ;; An important Go! operator
	   ((looking-at go-operators-regex)

	    (let* ((symbol (intern (match-string 0)))
		   (symbol-prec (get symbol 'precedence)))

	      ;; Check to see if we should pop any operators off the stack
	      (if (get symbol 'pop)
		  ;; Yes, pop off any lower precedence operators
		  (while (<= tos-prec symbol-prec)
		    (setq state (car stack)
			  stack (cdr stack)
			  tos-prec   (first state)
			  tos-op     (second state)
			  tos-indent (third state))))
	    
	      (if (get symbol 'pop-until-same)
		  ;; Yes, pop of all operators until
		  ;; we meet an operator with the same
		  ;; precedence (for brackets)
		  (progn
		    (while (and (/= tos-prec symbol-prec) (cdr stack))
		      (setq state (car stack)
			    stack (cdr stack)
			    tos-prec   (first state)
			    tos-op     (second state)
			    tos-indent (third state)))
		    (setq state (car stack)
			  stack (cdr stack)
			  tos-prec   (first state)
			  tos-op     (second state)
			  tos-indent (third state))))

	      ;; Push the symbol onto the stack, if allowed
	      (if (get symbol 'push)
		  (progn
		    (setq 
		     ;; Save the old state
		     state (list tos-prec 
				 tos-op 
				 tos-indent)
		     ;; Push it onto the stack
		     stack (cons state stack) 
		     ;; New top-of-stack (indentation carries on
		     ;; from before)
		     tos-prec   symbol-prec
		     tos-op     symbol)))
	    
	      ;; Advance the pointer 
	      (forward-char (get symbol 'length))

	      ;; Adjust the indentation for hanging
	      (if (and (get symbol 'hanging)
		       (not (looking-at "[ \t]*\\(--[ \t]?\\)?$")))
		  ;; Hanging
		  (progn 
		    (skip-chars-forward " \t")
		    (setq tos-indent 
			  (+ tos-indent
			     (- (current-column)
				(max (go-indentation-level (point))
				     (third (car stack)))))))

		;; Not Hanging
		(setq tos-indent (+ tos-indent 
				    (eval (get symbol 'delta)))))
	      ))
	 
	   ;; Skip escaped characters
	   ((looking-at "`\\\\.") (forward-char 3))

	   ;; Skip syntax
	   ((looking-at go-line-comment)
	    (go-skip-line-comment))
	   ((looking-at go-body-comment)
	    (let ((co-col (current-column)))
	      (go-skip-block-comment)
	      (if (>= (point) to)
		  (setq tos-indent (1+ co-col)
			tos-in-comment t))))
	   ((looking-at go-escaped-string-regex)
	    (forward-char 2))
	   ((looking-at "\"")
	    (go-skip-string))
	   ((looking-at "\'")
	    (go-skip-symbol))
	   ((looking-at "`")
	    (go-skip-char))
	   (t 
	    ;; It might be better to forward char first and then scan
	    ;; for the next token to eliminate any possibility of
	    ;; an un-handled token.
	    (or (and (search-forward-regexp go-next-token-regex to t)
		     (goto-char (match-beginning 0)))
		(forward-char)
		)))
	  (skip-chars-forward " \t\n"))

	;; Save the state for future runs
	(setq state (list tos-prec 
			  tos-op 
			  tos-indent
			  tos-in-comment))
	(go-debug "stack: %s %s\n" state stack)
	(cons state stack)))))

(defun go-vertical-bar-adjust (pos bar)
  "Returns the number of columns occupied by the | and following spaces"
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    (if (looking-at (concat bar "[ \t]*[^ \t\n\r]"))
	(progn
	  (forward-char)
	  (1+ (skip-chars-forward " \t")))
      1)))

(defun go-calculate-indent (pos)
  (save-excursion
    (goto-char pos)
    (beginning-of-line)
    (skip-chars-forward " \t")
    
    (cond
     ;; Keep comments at beginning of line at the beginning
     ((looking-at go-comment-bol) 0)

     ;; Otherwise indent to standard indent position
     ((looking-at go-comment)
      (go-calculate-brace-indent pos))

     ;; If it's a close brace then we can short-cut (a bit)
     ((looking-at go-close-par)
      (go-calculate-outer-indent (point)))

     ((looking-at "\\.}")
      (go-calculate-outer-indent (1+ (point))))

     ;; If it's a | we need to parse past it to get the
     ;; real indentation level 
     ;; (this method would work fine for close braces as well)
     ((looking-at "[|?]")
      (- (go-calculate-brace-indent 
	  (go-line-get-pos-after pos "[|?]"))
	 (go-vertical-bar-adjust pos "[|?]")))
     
     ;; Otherwise standard indent position
     (t 
      (go-calculate-brace-indent pos)))))

(defun go-goto-first-non-whitespace-maybe ()
  (let ((dest (save-excursion
		(beginning-of-line)
		(skip-chars-forward " \t")
		(point))))
    (if (< (point) dest)
	(goto-char dest))))

(defun go-debug (msg &rest args)
  "Print a debug message to the *go-debug* buffer"
  (if go-debugging
      (save-excursion
	(set-buffer (get-buffer-create "*go-debug*"))
	(insert (apply 'format msg args)))))
  
;;; Hook called when the tab key is pressed
(defun go-indent-line ()
  (save-excursion
    (let* ((bol         (progn (beginning-of-line) (point)))
	   (cur-level   (go-indentation-level bol))
	   (level       (go-calculate-indent bol)))
      (if (= cur-level level)
	  nil
	(progn
	  (delete-horizontal-space)
	  (indent-to level)
	  ;; (go-readjust-comment bol)
	  ))))
  (go-goto-first-non-whitespace-maybe))

;;; Readjust a -- comment on the same line
;;; (not used for now)
(defun go-readjust-comment (pos)
  "readjust a line comment if there is one on the current line"
  (save-excursion
    (let
	((bol (progn (goto-char pos)(beginning-of-line)(point)))
	 (eol (progn (goto-char pos)(end-of-line)(point))))
      (goto-char bol)
      (cond ((search-forward-regexp comment-start-skip eol t)
	     (indent-for-comment))))))

(defun go-indent-sexp ()
  (interactive)
  (save-excursion
    (let (;(start  (point))
	  (stop   (condition-case nil
		      (save-excursion (forward-sexp 1) (point))
		    (error (point)))))
      (while (and (< (point) stop)
		  (progn (go-indent-line) t)
		  (eq (forward-line) 0)))
      (go-indent-line))))

;;; Font-lock support

(defvar go-font-lock-keyword-regexp 
  (concat "\\b\\("
	  (go-one-of 
	   "import"			; package
           "private"                    ; non-exported element of package
	   "action"			; control
	   "valof"			; control
	   "valis"			; control
	   "istrue"			; control

	   "logical"			; type
	   "void"			; type
	   "symbol"			; type
	   "char"			; type
	   "number"			; type 
	   "float"			; type 
	   "integer"			; type 
	   "opaque"			; type
	   "thread"			; type

	   "rem"			; arithmetic operator
	   "quot"			; arithmetic operator

	   "true"			; standard enumeration symbol
	   "false"			; standard enumeration symbol

	   "this"			; this object

	   "timeout"			;
	   "string"			; type
           "sync"
	   "spawn"			; control
	   "onerror"			; control
	   "in"				; control
           "case"                       ; control

	   "raise"			; control
	   "error"			; standard constructor
           )
	  "\\)\\b")
  "Regular expression matching the keywords to highlight in Go mode")

;;; I think that there is too much highlighting
;;; perhaps just highlight arrows => --> -> :- :-- ?

(defvar go-font-lock-symbol-regexp
  (concat "\\("
	  (go-one-of 
	   "::="
	   "\\$="
	   "\\$"
	   "=>"
	   "-->"
	   ":--"
	   "->"
	   "<="
	   "{\\."
	   "\\.}"
	   "\\.\\."
	   ":="
	   "\\.="
	   "%="
	   ">="
	   "=="
	   "=<"
	   "="
	   "<\\~"
	   "<>"
	   "\\*>"
	   "::="
	   "::"
	   ":"
	   "%%"
	   "~"
	   "@="
	   "@>"
	   "@@"
	   "@"
	   "#"
	   "\\^"
	   "\\^\\^"
	   "\\\\\\+"
	   "\\\\="
	   ",\\.\\."
	   "!\\."
	   "\\."
	   "!"
	   "-+"
	   "+"
	   "-")
	  "\\)")
  "Regular expression matching the symbols to highlight in Go mode")

(defvar go-font-lock-function-regexp
  "^[ \t]*\\(\\sw+\\)([][0-9_a-zA-Z?,.:`'\\ ]*)[ \t]*\\([=-]+>\\|:-\\)"
  "Regular expression matching the function declarations to highlight in Go mode")

(defvar go-font-lock-include-regexp
  "import[ \t]+"
  "Regular expression matching the compiler import package statement")

(defvar go-font-lock-comment-regexp-bol
  "^\\(--[ \t].*$\\)")

(defvar go-font-lock-comment-regexp
  "[^:]\\(--[ \t].*$\\)")

;; Match a line comment, not inside a string.
(defun go-match-line-comment (limit)
  (let ((from (save-excursion (beginning-of-line) (point))))
    (if (search-forward-regexp go-font-lock-comment-regexp limit t)
	(let ((state (parse-partial-sexp from (match-beginning 1))))
	  (if state
	      (if (nth 3 state)
		  (go-match-line-comment limit)
		t)
	    t)))))

(defconst go-dot-space (intern ". "))
(defconst go-dot-newline (intern ".\n"))
(defconst go-dot-tab (intern ".\t"))

(defun go-match-function (limit)
  (if (search-forward-regexp "^[ \t]*\\(\\sw+\\)[ \t]*" limit t)
      (let* ((s (save-excursion 
		  (save-match-data 
		    (go-parse-until (progn (beginning-of-line) (point))))))
	     (op (go-parse-state-op s)))
	(cond
	 ((and (eq op '\{) (cdr s)
	       (not (eq (go-parse-state-op (cdr s)) 'action)))
	  t)
	 ((or (eq op go-dot-space) 
	      (eq op go-dot-newline)
	      (eq op go-dot-tab))
	  t)
	 (t
	  (go-match-function limit))))))

(defconst go-font-lock-keywords-1
  `((,go-font-lock-comment-regexp-bol (1 font-lock-comment-face))
    (,go-font-lock-comment-regexp     (1 font-lock-comment-face))
;;    (go-match-line-comment (1 font-lock-comment-face))
    (,go-font-lock-keyword-regexp     (1 font-lock-keyword-face))
    (,go-font-lock-symbol-regexp      (1 font-lock-reference-face))
;;;    (,go-font-lock-include-regexp     (1 font-lock-doc-string-face))
    (,go-font-lock-function-regexp    (1 font-lock-function-name-face))
    (go-match-function     (1 font-lock-function-name-face t))
    ))

(defvar go-font-lock-keywords go-font-lock-keywords-1
  "Keywords to syntax highlight with font-lock-mode")

(defun go-init-font-lock ()
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(go-font-lock-keywords nil nil nil nil)))

(provide 'go)
