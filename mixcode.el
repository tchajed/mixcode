(defun file-to-nlsep-strings (fname)
  "Read file FNAME, fontify the file contents, and produce a list of strings
separated by new lines."
  (with-temp-buffer
	;; Enable `visit' to set `buffer-file-name' to guide `set-auto-mode'.
	;; Not using `fild-file' (and friends) to allow closing buffers after read.
	(insert-file-contents fname t)
	(set-auto-mode)
	(font-lock-fontify-buffer)
	(let ((strings (split-string (buffer-string) "\n"))
		  (vbar (propertize "┃" 'face 'font-lock-comment-face)))
	  (mapcar (lambda (s) (format "%s %s" vbar s)) strings))))

(defun nlsep-strings-keywords (strings)
  (let (keywords)
	(dotimes (i (length strings))
	  (let ((matcher (format "\\((\\*@%d@\\*)\\)" (1+ i))) ; line number from 1
			(face `'(face nil display ,(nth i strings))))
		;; Set `override' to `t' to override major mode fontification
		(add-to-list 'keywords `(,matcher 0 ,face t))))
	keywords))

(defvar mixcode-source-dir nil)

(defun mixcode (fname)
  (interactive
   (list (read-file-name "Source file: " mixcode-source-dir)))
  (let* ((strings (file-to-nlsep-strings fname))
		 (keywords (nlsep-strings-keywords strings)))
	(font-lock-add-keywords nil keywords)
	(font-lock-fontify-buffer)))

;; (adapted from https://stackoverflow.com/a/44908362)
(defun parse-integer-list (str)
  "Parse string representing a range of integers into a list of integers."
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

(defun mixcode-insert-lines (str)
  "Inserting mixcode comments."
  (interactive "sLine numbers: ")
  (let ((lines (parse-integer-list str)))
	(dolist (line lines nil)
	  (insert (format "(\*@%d@\*)\n" line)))))

;; TODO: function to remove font lock keywords
