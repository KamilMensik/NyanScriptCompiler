(defun gen-token (string type &optional additional)
  `(token ,string ,type ,additional))

(defun token-value (token)
  (second token))

(defun token-type (token)
  (third token))

(defun token-additional (token)
  (fourth token))

(defun regionp (token)
  (eql (token-type token) :region))

;; Token format is ('token value type additional-data)
;; Additional data are used for conditional branching

(defmacro build-token (type)
  (labels ((get-info-for-build-token (type)
	     (gethash type *supported-types*)))
    (let ((info (get-info-for-build-token type)))  
      `(let* ((token-end (get-token-end line (1+ i) len #',(cadr info))))
	 (cons (gen-token (,(car info) (subseq line i token-end)) ,type)
	       (handle-line line len token-end))))))	 

(defun tokenize (file)
  (labels ((get-token-end (line j len cond)
	     (if (>= j len) j
		 (let ((c (aref line j)))
		   (cond
		     ((or (eql c #\Space) (eql c #\,)) j)
		     ((funcall cond c) (get-token-end line (1+ j) len cond))
		     (t (error "Compiler error"))))))
	   (handle-line (line len &optional (i 0) skip)
	     (if (< i len) (let ((c (aref line i)))
			     (cond
			       ((eql c #\/) (if skip nil (handle-line line len (1+ i) t)))
			       ((eql c #\,) (cons (gen-token "," :divider) (handle-line line len (1+ i))))
			       ((eql c #\:) (build-token :function))
			       ((eql c #\.) (build-token :region))
			       ((digit-char-p c) (build-token :number))
			       ((symbol-char-p c) (build-token :identifier))
			       (t (handle-line line len (1+ i)))))
		 (cons (gen-token nil :endline) nil))))
    (let ((line (read-line file nil)))
      (if line (append (handle-line (string-upcase line) (length line)) (tokenize file))
	  nil))))
