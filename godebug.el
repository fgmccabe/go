;;; 
;;; Go! debugging Emacs mode
;;; Copyright (C) 2006 F.G. McCabe

;;; Loosely based on gud.el, and others

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

(require 'comint)

(defvar godebug-last-frame)
(defvar godebug-thread-id)
(defvar godebug-delete-prompt-marker)
(defvar godebug-filter-accumulator "")
(defvar godebug-arrow-extent nil)

(defcustom godebug-key-prefix "\C-c"
  "Prefix of all godebug commands valid in Go! buffers."
  :type 'string
  :group 'godebug)

(make-face 'godebug-arrow-face)
(or (face-differs-from-default-p 'godebug-arrow-face)
   ;; Usually has a better default value than highlight does
   (copy-face 'isearch 'godebug-arrow-face))

(defvar godebug-prompt-pattern "^\\[.*\\] *(go.Debug)\\? *$"
  "A regexp to recognize the prompt for input from go.debug.") 

(defvar godebug-mode-map nil
  "Keymap for godebug-mode.")

(defvar godebug-go-options '("-h" "2000")
  "List of command-line options to give to go engine for debugging programs"
)

(if godebug-mode-map
   nil
  (setq godebug-mode-map (make-sparse-keymap "Go Debug keymap"))
  (set-keymap-parent godebug-mode-map comint-mode-map)
  (define-key godebug-mode-map "\C-l" 'godebug-refresh)
  (define-key godebug-mode-map "\C-c" 'godebug-control-c-subjob)
  (define-key godebug-mode-map " " 'godebug-break)
  (global-set-key (concat godebug-key-prefix " ") 'godebug-break)
  (define-key godebug-mode-map "S-v" 'godebug-showvar))


;; Define a command to be sent to the debugger

(defmacro godebug-cmd (func str key &optional doc)
  "Define FUNC to be a command sending STR and bound to KEY, with
optional doc string DOC."

  (list 'progn
	(list 'defun func '(arg)
	      (or doc "")
	      '(interactive "p")
	      (list 'godebug-call str 'arg))
	(if key
	    (list 'define-key
		  'godebug-mode-map
		  key
		  (list 'quote func)))
	(if key
	    (list 'global-set-key
		  (list 'concat 'godebug-key-prefix key)
		  (list 'quote func)))))

;; Commands understood by the debugger
(godebug-cmd godebug-next "n" "n" "Step over source line with display")

(godebug-cmd godebug-step "s" "s" "Step into")

(godebug-cmd godebug-quit "q" "q" "Terminate debugged process")

(godebug-cmd godebug-trace "t" "t" "Go into trace mode")

(godebug-cmd godebug-cont "c" "c" "Go into continue mode")

(godebug-cmd godebug-vars "v" "v" "Show all variables")

(godebug-cmd godebug-show "x" "x" "Show current call")

(godebug-cmd godebug-show-0 "0" "0" "Show program name")

(godebug-cmd godebug-show-1 "1" "1" "Show 1st argument")
(godebug-cmd godebug-show-2 "2" "2" "Show 2nd argument")
(godebug-cmd godebug-show-3 "3" "3" "Show 3rd argument")
(godebug-cmd godebug-show-4 "4" "4" "Show 4th argument")
(godebug-cmd godebug-show-5 "5" "5" "Show 5th argument")
(godebug-cmd godebug-show-6 "6" "6" "Show 6th argument")
(godebug-cmd godebug-show-7 "7" "7" "Show 7th argument")
(godebug-cmd godebug-show-8 "8" "8" "Show 8th argument")
(godebug-cmd godebug-show-9 "9" "9" "Show 9th argument")


(defun godebug-showvar (arg)
  "Ask the debugger to show a single variable"
  (interactive "sShow variable ")
  (godebug-call (concat "v " arg)))


(defvar godebug-display-mode nil
  "Minor mode for gdb frame display")
(or (assq 'godebug-display-mode minor-mode-alist)
    (setq minor-mode-alist
	  (purecopy
	   (append minor-mode-alist
		   '((godebug-display-mode " Frame"))))))

(defun godebug-display-mode (&optional arg)
  "Toggle Frame display mode
With arg, turn display mode on if and only if arg is positive.
In the display minor mode, source file are displayed in another
window for repective \\[godebug-display-frame] commands."
  (interactive "P")
  (setq godebug-display-mode (if (null arg)
			     (not godebug-display-mode)
			   (> (prefix-numeric-value arg) 0))))


(defun godebug-mode ()
  "Major mode for interacting with an inferior go debugging process.
The following commands are available:

\\{godebug-mode-map}

\\[godebug-display-frame] displays in the other window
the last line referred to in the gdb buffer. See also
\\[godebug-display-mode].

\\[godebug-step] and \\[godebug-next]
call godebug to step into or over and then update the other window
with the current file and position.

If you are in a source file, you may select a point to break
at, by doing \\[godebug-break].

Commands:
Many commands are inherited from comint mode. 
Additionally we have:

\\[godebug-display-frame] display frames file in other window
\\[godebug-step] advance one line in program

C-x SPACE sets break point at current line."
  (interactive)
  (comint-mode)
  (use-local-map godebug-mode-map)
  (make-local-variable 'godebug-last-frame)
  (make-local-variable 'godebug-thread-id)
  (make-local-variable 'godebug-delete-prompt-marker)
  (make-local-variable 'godebug-display-mode)
  (make-local-variable' godebug-filter-accumulator)
  (setq godebug-last-frame nil
        godebug-delete-prompt-marker nil
        godebug-filter-accumulator ""
	godebug-display-mode t
        major-mode 'godebug-mode
        mode-name "Go! Debug"
        comint-prompt-regexp godebug-prompt-pattern)
  (setq godebug-arrow-extent nil)
  (run-hooks 'godebug-mode-hook))

(defvar current-godebug-buffer nil)

;;;###autoload
(defvar godebug-command-name "/opt/go/bin/go"
  "Pathname for executing go in debug mode")

;;;###autoload
(defun godebug-buffer (bfr arg)
"Try to run the debugger on the program in this buffer"
  (interactive "bDebug package in: 
sArguments to program (optional) ")
  (switch-to-buffer bfr)
  (let (path pkg)
    (if (eq major-mode 'go-mode)
	(if (string-match
	   ;; Extract the directory and package name
	     "\\(.*\\)/\\([^/.]+\\)\\.go"
	     (buffer-file-name))
	    (progn
	      (setq path (substring (buffer-file-name)
				    (match-beginning 1)
				    (match-end 1))
		    pkg (substring (buffer-file-name)
				   (match-beginning 2)
				   (match-end 2)))
	      (godebug path pkg (tokenize arg)))
	  (message "Not a Go! package"))
      (message "Not a Go! mode buffer")))
  )

(defun tokenize (arg)
  "Split a string into a list of arguments, separated by spaces"
  (let ((output nil))
    (progn
      (while (string-match 
	      " *\\([^ ]+\\)" arg)
	(setq output (cons (substring arg (match-beginning 1)
				      (match-end 1))
			   output)
	      arg (substring arg (match-end 0)))
	)
      (nreverse output)
      )
    )
  )


;;;###autoload
(defun godebug (path pkg &optional args)
  "Run godebug on PKG in buffer *godebug-PKG*.
If PATH is present then add path to the class path"
  (interactive "FRun godebug on package:
sOptional arguments: ")
  (setq path (file-truename (expand-file-name path)))
  (let (bfr-proc)
    (switch-to-buffer (concat "*godebug-" pkg "*"))
    (or (bolp) (newline))
    (insert "... debugging package " pkg "\n")
    (insert "Current directory is " path "\n")
    (setq bfr-proc (get-buffer-process 
		    (eval (append 
			   (list 'make-comint
				 (concat "godebug-" pkg)
				 godebug-command-name
				 nil
				 "-g"
				 "-P" path
				 "-h" "2000"
				 pkg)
			   args)
			  )))
    (set-process-filter bfr-proc 'godebug-filter)
    (set-process-sentinel bfr-proc 'godebug-sentinel)
    ;; XEmacs change: turn on godebug mode after setting up the proc filters
    (godebug-mode)
    (godebug-set-buffer)))

(defun godebug-set-buffer ()
  (cond ((eq major-mode 'godebug-mode)
	 (setq current-godebug-buffer (current-buffer)))))

;; This function is responsible for collecting output from the 
;; debugger process and inserting it into the console buffer.
;; Aside from inserting the text, it notices and deletes
;; each filename-and-line-number;
;; It records the filename and line number, and maybe displays that file.

(defvar godebug-defer-flag nil
  "Non-nil means don't process anything from the debugger right now.
It is saved for when this flag is not set.")

(defvar godebug-pending-text nil
  "Non-nil means this is text that has been saved for later")

(defun godebug-filter (proc string)
  ;; Here's where the actual buffer insertion is done
  (let (output process-window)
    (if (buffer-name (process-buffer proc))
	(if godebug-defer-flag
	    ;; If we can't process any text now,
	    ;; save it for later.
	    (setq godebug-pending-text
		  (concat (or godebug-pending-text "") string))

	  ;; If we have to ask a question during the processing,
	  ;; defer any additional text that comes from the debugger
	  ;; during that time.
	  (let ((godebug-defer-flag t))
	    ;; Process now any text we previously saved up.
	    (if godebug-pending-text
		(setq string (concat godebug-pending-text string)
		      godebug-pending-text nil))
	    (save-excursion
	      (set-buffer (process-buffer proc))
	      ;; Save the process output, checking for source file markers.
	      (setq output (godebug-marker-filter string))
	      ;; Let the comint filter do the actual insertion.
	      ;; That lets us inherit various comint features.
	      (comint-output-filter proc output))

	    ;; Put the arrow on the source line.
	    ;; This must be outside of the save-excursion
	    ;; in case the source file is our current buffer.
	    (if process-window
		(save-selected-window
		  (select-window process-window)
		  (godebug-display-frame))
	      ;; We have to be in the proper buffer, (process-buffer proc),
	      ;; but not in a save-excursion, because that would restore point.
	      (let ((old-buf (current-buffer)))
		(set-buffer (process-buffer proc))
		(unwind-protect
		    (godebug-display-frame)
		  (set-buffer old-buf)))))

	  ;; If we deferred text that arrived during this processing,
	  ;; handle it now.
	  (if godebug-pending-text
	      (godebug-filter proc godebug-pending-text))))))

;; Process output from the debugger, stripping out the stuff we handle
;; automatically
(defun godebug-marker-filter (string)
  (let ((output "") file-found)
    (setq godebug-filter-accumulator
	  (if godebug-filter-accumulator
	      (concat godebug-filter-accumulator string)))
      
    (while 
	(string-match
	 ;; The debugger prompts with 
	 ;; [<thread>] line <file>@<number>
	 "^\\(\\[[^]]+\\]\\) +line +file:\\([^:]*\\):\\([0-9]+$\\)"
	 godebug-filter-accumulator)
      (setq godebug-thread-id (substring godebug-filter-accumulator 
					 (match-beginning 1)
					 (match-end 1)))
      (setq output (concat output 
			   (substring godebug-filter-accumulator 0
				      (match-beginning 0))))
      (setq file-found 
	    (substring godebug-filter-accumulator 
		       (match-beginning 2)
		       (match-end 2)))
      (setq godebug-last-frame
	    (cons file-found
		  (string-to-number
		   (substring godebug-filter-accumulator 
			      (match-beginning 3)
			      (match-end 3)))))
;;      (message (concat "thread is " godebug-thread-id))
;;      (message (concat "file is " file-found))
;;      (message (concat "line number is " (substring godebug-filter-accumulator 
;;						    (match-beginning 3)
;;						    (match-end 3))))
      (setq godebug-filter-accumulator 
	    (substring godebug-filter-accumulator (match-end 0))))
    (if (string-match "^\\(\\[[^]]+\\]\\) +line" godebug-filter-accumulator) 
	(setq output (concat output 
			     (substring godebug-filter-accumulator
					0
					(match-beginning 0)))
	      godebug-filter-accumulator 
	      (substring godebug-filter-accumulator
			 (match-beginning 0)))
      (setq output (concat output godebug-filter-accumulator)
	    godebug-filter-accumulator ""))
;;    (message (concat "remaining input is " godebug-filter-accumulator))
    output)
  )

(defun godebug-sentinel (proc msg)
  (cond ((null (buffer-name (process-buffer proc)))
	 ;; buffer killed
	 (set-process-buffer proc nil))
	((memq (process-status proc) '(signal exit))
	 ;; (debug)
	 ;; Stop displaying an arrow in source file.
	 (if overlay-arrow-position
	     (progn
	       (set-marker overlay-arrow-position nil)
	       (setq overlay-arrow-position nil)))
	 ;; Fix the mode line.
	 (assq-delete-all 'godebug-display-mode minor-mode-alist)
;;	 (setq modeline-process
;;	       (concat ": go " (symbol-name (process-status proc))))
	 (let* ((obuf (current-buffer)))
	   ;; save-excursion isn't the right thing if
	   ;;  process-buffer is current-buffer
	   (unwind-protect
	       (progn
		 ;; Write something in *compilation* and hack its mode line,
		 (set-buffer (process-buffer proc))
		 ;; Force mode line redisplay soon
		 (set-buffer-modified-p (buffer-modified-p))
		 (if (eobp)
		     (insert ?\n mode-name " " msg)
		   (save-excursion
		     (goto-char (point-max))
		     (insert ?\n mode-name " " msg)))
		 ;; If buffer and mode line will show that the process
		 ;; is dead, we can delete it now.  Otherwise it
		 ;; will stay around until M-x list-processes.
		 (delete-process proc))
	     ;; Restore old buffer, but don't restore old point
	     ;; if obuf is the gdb buffer.
	     (set-buffer obuf))))))

(defun godebug-refresh (&optional arg)
  "Fix up a possibly garbled display, and redraw the arrow."
  (interactive "P")
  (recenter arg)
  (godebug-display-frame))

(defun godebug-display-frame (&optional nodisplay noauto)
  "Find, obey and delete the last filename-and-line marker 
Obeying it means displaying in another window the specified file and line."
  (interactive)
  (godebug-set-buffer)
  (and godebug-last-frame (not nodisplay)
       godebug-display-mode
       (godebug-display-line (car godebug-last-frame) (cdr godebug-last-frame))))

(defun godebug-display-line (true-file line)
  (let* ((last-nonmenu-event t)	 ; Prevent use of dialog box for questions.
	 (use-dialog-box nil) ; XEmacs
	 (buffer
	  (save-excursion
	    (or (eq (current-buffer) current-godebug-buffer)
		(set-buffer current-godebug-buffer))
	    (godebug-find-file true-file)))
	 (window (and buffer (or (get-buffer-window buffer)
				 (display-buffer buffer))))
	 (pos))
    (if buffer
	(progn
	  (save-excursion
	    (set-buffer buffer)
	    (save-restriction
	      (widen)
	      (goto-line line)
	      (setq pos (point))
	      (setq overlay-arrow-string "=>")
	      (or overlay-arrow-position
		  (setq overlay-arrow-position (make-marker)))
	      (set-marker overlay-arrow-position (point) (current-buffer)))
	    (cond ((or (< pos (point-min)) (> pos (point-max)))
		   (widen)
		   (goto-char pos))))
	  (set-window-point window overlay-arrow-position)))))

(defun godebug-find-file (file)
  (save-excursion
    (while (string-match "//+" file)
    (setq file (replace-match "/" t t file)))

    (let ((buf (find-file-noselect file)))
      (set-buffer buf)
      buf)))

(defun godebug-call (command &optional arg)
  "Invoke godebug COMMAND displaying source in other window."
  (interactive)
  (godebug-set-buffer)

  (goto-char (point-max))
  ;; Record info on the last prompt in the buffer and its position.
  ;; This is used in  godebug-maybe-delete-prompt
  ;; to prevent multiple prompts from accumulating.
  (save-excursion
    (goto-char (process-mark (get-buffer-process current-godebug-buffer)))
    (let ((pt (point)))
      ;(beginning-of-line)
      (forward-line 0)
      (setq godebug-delete-prompt-marker
	    (if (= (point) pt)
		nil
	      (list (point-marker) (- pt (point))
		    (buffer-substring (point) pt)))))
    (godebug-maybe-delete-prompt))
  (process-send-string (get-buffer-process current-godebug-buffer)
		       (concat command "\n")))

(defun godebug-maybe-delete-prompt ()
  (if godebug-delete-prompt-marker
      ;; Get the string that we used as the prompt before.
      (let ((prompt (nth 2 godebug-delete-prompt-marker))
	    (length (nth 1 godebug-delete-prompt-marker)))
	;; Position after it.
	(goto-char (+ (car godebug-delete-prompt-marker) length))
	;; Delete any duplicates of it which follow right after.
	(while (and (<= (+ (point) length) (point-max))
		    (string= prompt
			     (buffer-substring (point) (+ (point) length))))
	  (delete-region (point) (+ (point) length)))
	;; If that didn't take us to where output is arriving,
	;; we have encountered something other than a prompt,
	;; so stop trying to delete any more prompts.
	(if (not (= (point)
		    (process-mark (get-buffer-process current-godebug-buffer))))
	    (progn
	      (set-marker (car godebug-delete-prompt-marker) nil)
	      (setq godebug-delete-prompt-marker nil))))))

(defun godebug-break (temp)
  "Set breakpoint at this source line."
  (interactive "P")
  (let* ((line (save-restriction
		 (widen)
		 (beginning-of-line)
		 (1+ (count-lines 1 (point)))))
	 (cmd (concat "b \"" (buffer-file-name) "\":" (int-to-string line))))
    (godebug-call cmd)
    )
  )

(defun godebug-clear ()
  "Clear breakpoint at this source line."
  (interactive "P")
  (let* ((line (save-restriction
		 (widen)
		 (beginning-of-line)
		 (1+ (count-lines 1 (point)))))
	 (cmd (concat "B \"" (buffer-file-name) "\":" (int-to-string line))))
    (godebug-call cmd)
    )
  )

(fset 'godebug-control-c-subjob 'comint-interrupt-subjob)

;(defun godebug-control-c-subjob ()
;  "Send a Control-C to the subprocess."
;  (interactive)
;  (process-send-string (get-buffer-process (current-buffer))
;		       "\C-c"))

(provide 'godebug)