(require 'go-mode)

(defvar mixcode-source-dir     nil)
(defvar mixcode-source-file    nil)
(defvar mixcode-source-strs    nil)
(defvar mixcode-source-functbl nil)

;;; Fontification

(defun mixcode-fontify-source-string (str)
  (with-temp-buffer
	(insert str)
	(go-mode)
	(font-lock-fontify-buffer)
	(let ((vbar (propertize "┃" 'face 'font-lock-comment-face)))
	  (format "%s %s" vbar (buffer-string)))))

(defun mixcode-fontify-buffer ()
  (let ((keywords '(("(\\*@[[:blank:]]\\(.*\\)[[:blank:]]@\\*)"
					 0
					 `(face
					   nil
					   display
					   ;; fontify the first match subexpression
					   ;; (i.e., inside comment notation)
					   ,(mixcode-fontify-source-string (match-string 1)))
					 ;; set `override' to override major mode fontification
					 t))))
	;; TODO: set `'font-lock-extra-managed-props' for resetting fontification
	(font-lock-add-keywords nil keywords)
	(font-lock-fontify-buffer)))

;;; Comment generation


;; (adapted from https://stackoverflow.com/a/44908362)
(defun mixcode-parse-line-numbers (str)
  "Parse string STR representing a range of integers into a list of integers."
  (let (start ranges)
    (while (string-match "\\([0-9]+\\)\\(?:-\\([0-9]+\\)\\)?" str start)
      (push
       (apply 'number-sequence
              (seq-map 'string-to-number
                       (seq-filter
                        'identity
                        (list (match-string 1 str) (match-string 2 str)))))
       ranges)
      (setq start (match-end 0)))
    (nreverse (seq-mapcat 'nreverse ranges))))

(defun mixcode-insert-lines (lines)
  (dolist (line lines nil)
	(let ((str (nth (1- line) mixcode-source-strs)))
	  (when str (insert (format "(\*@ %s @\*)\n" str))))))

(defun mixcode-insert-code-with-numbers (str)
  "Insert commented code based on line numbers."
  (interactive
   (list (progn
		   (unless mixcode-source-strs (error "Run `mixcode-load-file' first."))
		   (read-string "Line numbers: "))))
  (let ((lines (mixcode-parse-line-numbers str)))
	(mixcode-insert-lines lines)))

(defun mixcode-insert-code (str)
  "Insert commented code based on function name."
  (interactive
   (list (progn
		   (unless mixcode-source-strs (error "Run `mixcode-load-file' first."))
		   (read-string "Function name: "))))
  (princ mixcode-source-functbl)
  (let ((range (gethash str mixcode-source-functbl)))
	(unless range (error "Unknown function name."))
	(mixcode-insert-lines (number-sequence (car range) (cdr range)))))

;;; Load and process source file

(defun mixcode-build-functbl ()
  (goto-char (point-min))
  (let ((tbl (make-hash-table :test 'equal)))
	(while (search-forward-regexp "^func \\(.*\\)(\\(.*\\))" nil t)
	  (let ((func (match-string 1))
			(begin  (line-number-at-pos)))
		(when (search-forward-regexp "^}" nil t)
		  (puthash func (cons begin (line-number-at-pos)) tbl))))
	tbl))

(defun mixcode-build-strs-functbl (fname)
  (let ((built (with-temp-buffer
				 (insert-file-contents fname)
				 (cons
				  (split-string (buffer-string) "\n")
				  (mixcode-build-functbl)))))
	(setq-local mixcode-source-strs (car built))
	(setq-local mixcode-source-functbl (cdr built))))

(defun mixcode-read-source-file ()
  (read-file-name "Source file: "
				  mixcode-source-dir
				  nil nil
				  mixcode-source-file))

(defun mixcode-load-file (fname)
  (interactive
   (list (mixcode-read-source-file)))
  (mixcode-build-strs-functbl fname))

(define-minor-mode
  mixcode-mode
  "Mixing Go code within Coq proof."
  :lighter " mixcode"
  (mixcode-fontify-buffer))
