(require 'go-mode)

(defvar mixcode-source-dir  nil)
(defvar mixcode-source-file nil)
(defvar mixcode-source-strs nil)
(defvar mixcode-source-tbl  nil)

;;; Fontification

(defun mixcode-fontify-source-string (str)
  (with-temp-buffer
	(insert str)
	(go-mode)
	(font-lock-fontify-buffer)
	(let ((vbar (propertize "┃" 'face 'font-lock-comment-face)))
	  (format "%s %s" vbar (buffer-string)))))

(defun mixcode-fontify-begin-line ()
  (propertize (format "┏%s" (make-string 71 ?━)) 'face 'font-lock-comment-face))

(defun mixcode-fontify-end-line ()
  (propertize (format "┗%s" (make-string 71 ?━)) 'face 'font-lock-comment-face))

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
					 t)
					("(.*mixcode-begin.*)"
					 0
					 `(face
					   nil
					   display
					   ,(mixcode-fontify-begin-line))
					 t)
					("(.*mixcode-end.*)"
					 0
					 `(face
					   nil
					   display
					   ,(mixcode-fontify-end-line))
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

(defun mixcode-insert-begin ()
  (insert "(\*@@@@@@@@@@@@@@mixcode-begin@@@@@@@@@@@@@@\*)\n"))

(defun mixcode-insert-end ()
  (insert "(\*@@@@@@@@@@@@@@@mixcode-end@@@@@@@@@@@@@@@\*)\n"))

(defun mixcode-insert-qed ()
  (insert "Qed.\n"))

(defun mixcode-insert-lines (lines)
  (dolist (line lines nil)
	(let ((str (nth (1- line) mixcode-source-strs)))
	  (when str (insert (format "(\*@ %s @\*)\n" str))))))

(defun mixcode-insert-code-with-numbers (str)
  "Insert commented code based on line numbers STR."
  (interactive
   (list (progn
		   (unless mixcode-source-strs (error "Run `mixcode-load-file' first."))
		   (read-string "Line numbers (example: 3,5-10): "))))
  (let ((lines (mixcode-parse-line-numbers str)))
	(mixcode-insert-lines lines)))

(defun mixcode-insert-code (sig)
  "Insert commented code based on function signature FUNC."
  (interactive
   (list (progn
		   (unless mixcode-source-strs (error "Run `mixcode-load-file' first."))
		   (completing-read "Function or type name: " mixcode-source-tbl))))
  (let ((range (gethash sig mixcode-source-tbl)))
	(unless range (error "Unknown function name."))
	(let ((begin (point)))
	  (mixcode-insert-begin)
	  (mixcode-insert-lines (number-sequence (car range) (cdr range)))
	  (mixcode-insert-end)
	  (indent-region begin (point)))))

(defun mixcode-insert-wp (sig)
  "Insert wp theorem."
  (interactive
   (list (progn
		   (unless mixcode-source-strs (error "Run `mixcode-load-file' first."))
		   (completing-read "Function name: " mixcode-source-tbl))))
  (let ((range (gethash sig mixcode-source-tbl)))
	(unless range (error "Unknown function name."))
	(mixcode-insert-theorem sig)
	(let ((begin (point)))
	  (mixcode-insert-begin)
	  (mixcode-insert-lines (number-sequence (car range) (cdr range)))
	  (mixcode-insert-end)
	  (indent-region begin (point)))
	(mixcode-insert-qed)))

;;; Load and process source file

(defun mixcode-build-tbl ()
  (goto-char (point-min))
  (let ((tbl (make-hash-table :test 'equal)))
	(while (search-forward-regexp "\\(^func .*\\|^type .*\\) {" nil t)
	  (let ((func  (string-trim (match-string 1)))
			(begin (line-number-at-pos)))
		(when (search-forward-regexp "^}" nil t)
		  (puthash func (cons begin (line-number-at-pos)) tbl))))
	tbl))

(defun mixcode-build-strs-tbl (fname)
  (let ((built (with-temp-buffer
				 (insert-file-contents fname)
				 (cons
				  (split-string (buffer-string) "\n")
				  (mixcode-build-tbl)))))
	(setq-local mixcode-source-strs (car built))
	(setq-local mixcode-source-tbl (cdr built))))

(defun mixcode-read-source-file ()
  (read-file-name "Source file: "
				  mixcode-source-dir
				  nil nil
				  mixcode-source-file))

(defun mixcode-load-file (fname)
  (interactive
   (list (mixcode-read-source-file)))
  (mixcode-build-strs-tbl fname))

(define-minor-mode
  mixcode-mode
  "Mixing Go code within Coq proof."
  :lighter " mixcode"
  (mixcode-fontify-buffer))

;; TODOs
;; 1. Representation predicate generation
;; 2. Comments above


(defun mixcode-parse-signature (s)
  (string-match "^func *\\(([^()]+)\\|\\) +\\([^()]*\\)(\\([^()]*\\))\\(.*\\)" s)
  (let ((recv (match-string 1 s))
		(func (match-string 2 s))
		(args (match-string 3 s))
		(rets (match-string 4 s)))
	(list (mixcode-parse-recv recv)
		  func
		  (mixcode-parse-args args)
		  (mixcode-parse-rets rets))))

(defun mixcode-parse-recv (s)
  (if (string-match "(\\(.*\\) +\\(.*\\))" s)
	  ;; method
	  (cons (match-string 1 s) (match-string 2 s))
	;; function
	nil))

(defun mixcode-parse-args (s)
  (let ((seps
		 (reverse (mapcar
				   (lambda (x) (split-string x " " t))
				   ;; each commas separate two vars; the regexp trims away spaces
				   (split-string s "," t "[[:space:]]*"))))
		(args)
		(type))
	(dolist (elem seps args)
	  (if (cdr elem)
		  ;; has its own type
		  (progn
		   (add-to-list 'args (cons (car elem) (nth 1 elem)))
		   (setq type (nth 1 elem)))
		;; does not have its own type; use the type of the previous variable
		(add-to-list 'args (cons (car elem) type))))))

(defun mixcode-parse-rets (s)
  (split-string s "[ ,]+" t "[()]"))

(defun mixcode-gotype-to-coqtype (type)
  (cond ((equal type "uint8")         "u8")
		((equal type "uint32")        "u32")
		((equal type "uint64")        "u64")
		((equal type "bool")          "bool")
		((equal type "string")        "string")
		((string-match "\\*.+" type)  "loc")
		((string-match "\\[].+" type) "Slice.t")
		(t                            "?type")))

(defun mixcode-type-var (var)
  (format " (%s : %s)" (car var) (mixcode-gotype-to-coqtype (cdr var))))

(defun mixcode-type-vars (vars)
  (seq-reduce (lambda (s var) (concat s (mixcode-type-var var))) vars ""))

(defun mixcode-name-rets (rets)
  (let ((names
		 (mapcar (lambda (n) (format "?v%d" n)) (number-sequence 1 (length rets)))))
	(seq-mapn #'cons names rets)))

(defun mixcode-toval-formatter (type)
  (cond ((equal type "uint8")         " #%s")
		((equal type "uint32")        " #%s")
		((equal type "uint64")        " #%s")
		((equal type "bool")          " #%s")
		;; perennial seems to miss coercion of LitString?
		((equal type "string")        " #(LitString %s)")
		((string-match "\\*.+" type)  " #%s")
		((string-match "\\[].+" type) " (to_val %s)")
		(t                            " (?to_val %s)")))

(defun mixcode-toval-var (var)
  (format (mixcode-toval-formatter (cdr var)) (car var)))

(defun mixcode-toval-vars (vars)
  (seq-reduce (lambda (s var) (concat s (mixcode-toval-var var))) vars ""))

(defun mixcode-insert-theorem (sig)
  ;; this would be ambiguous for reference/non-refererence receiver
  (let* ((p (mixcode-parse-signature sig))
		 (recv (nth 0 p))
		 (func (nth 1 p))
		 (args (nth 2 p))
		 (rets (nth 3 p))
		 (name (if recv (format "%s__%s" (string-trim (cdr recv) "\\*" nil) func) func))
		 (args-recv (if recv (cons recv args) args))
		 (typed-args (mixcode-type-vars args-recv))
		 ;; handle function without argument
		 (value-args (if args-recv (mixcode-toval-vars args-recv) " #()"))
		 (named-rets (mixcode-name-rets rets))
		 (typed-rets (if rets (format "%s," (mixcode-type-vars named-rets)) ""))
		 ;; handle function without return value
		 (value-rets (if rets (mixcode-toval-vars named-rets) " #()")))
	(insert
	 (format
	  ;; TODO: a better way is to use `indent-region', but right now the
	  ;; postcondition seems incorrectly indented, so manual indentation for now.
	  "Theorem wp_%s%s :\n  {{{ True }}}\n    %s%s\n  {{{%s RET%s; True }}}.\nProof.\n"
	  name typed-args name value-args typed-rets value-rets))))

;;; Test cases

;; (defconst f1 "func (tuple *Tuple) appendVersion(tid  uint64 , val ,  y string)")
;; (defconst f2 "func (tuple *Tuple) Free() ( A  ,  uint8 )")
;; (defconst f3 "func MkTuple() *Tuple")
;; (defconst f4 "func findVersion(tid uint64, vers []Version) Version")
;; (mixcode-parse-signature f2)
