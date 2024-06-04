(defmacro c-push (el)
  `(cond ((null ,el) nil)
	 ((atom ,el) (push ,el result))
	 (t (dolist (e ,el) (push e result)))))

;data parameter format (const type name value)
(defun valid-data-formatp (data)
  (and (cadr data) (caddr data) (cadddr data)))

(defun increment-cond-stack (amount stack)
  (labels ((increment-one (list)
	     (if (endp list) nil
		 (progn (incf (first (car list)) amount)))))
    (increment-one (first stack))
    (increment-one (second stack))))

(defun valid-value-typep (token-type type)
  (eql (gethash type *data-types*) token-type))

(defun handle-data-region (token data)
  (let ((token-type (token-type token))
        (token-value (token-value token))
        (constant (car data))
        (type (cadr data))
        (name (caddr data))
        (value (cadddr data)))
    (if (eql token-type :endline) (if (and type name value) :completed (if (or type constant) (error "Invalid syntax: Invalid data initialization") nil))
      (if (eql token-value 'const) (if (or constant type) (error "Invalid const detected in data region")
                                     '(t))
        (if type (if name (if value (error (format nil "Invalid data format for variable: ~A" name)) 
                              (if (valid-value-typep token-type type) (list constant type name token-value)
				  (error (format nil "Error: ~A is not of type ~A" token-value type))))
                   (list constant type (if (not (eql token-type :identifier)) (error "Invalid data region syntax") token-value)))
          (list constant (if (not (eql token-type :identifier)) (error "Invalid data region syntax") token-value)))))))

(defun parse-number-token (token cond-stack)
  (labels ((parse-int (number) ;; Parse int is 4 bytes long, 2 for command, 2 for data
	     (increment-cond-stack 2 cond-stack)
	     (list (mod number 256)
		   (floor number 256)
		   0
		   (static-typing  '(token :lnum :function)))))
    (let ((n (token-value token)))
      (cond ((< n 65536) (parse-int n)))))) ;; 2 byte number (Cant fit inside 2 byte command)

(defun parse-function-token (token cond-stack)
  (let ((res (list 0 (static-typing token))))  ;; 2 byte commands (first byte is additional data for command, second byte is for command code)
    (if res
	(if (eql (second res) :ignore) nil
		(prog1 res
		  (increment-cond-stack 1 cond-stack)
		  (case (token-value token)
		    (:if (push res (first cond-stack)))
		    (:else (pop (first cond-stack)) (push res (second cond-stack)))
		    (:then (pop (second cond-stack))))))
	(error (format nil "Unknown function: ~A" (token-value token))))))

(defun parse-identifier-token (token constants cond-stack)
  (let ((res (gethash (token-value token) constants)))
    (if res (case (gethash (token-type res) *data-types*)
	      (:number (parse-number-token res cond-stack)))
	(error (format nil "Unknown identifier: ~A" (token-value token))))))

(defmacro parsem (parse-fun)
  `(let ((res ,parse-fun))
     (parse (cdr tokens))
     (c-push res)))

(defun parse-tokens (tokens)
  (let ((constants (make-hash-table))
	(result '())
	(cond-stack (list '() '())))
    (labels ((parse (tokens &optional (region nil) (additional-data))
               (let ((token (car tokens)))
                 (cond ((null token) nil)
                       (region (if (regionp token) (if (not (eql (token-value token) '.end)) (error (format nil "Unexpected region. Previous region: ~A hasnt been properly closed" region))
                                                       (parse (cdr tokens) nil))
                                   (case region
                                     ('.data (let ((result (handle-data-region token additional-data)))
                                               (if (eql result :completed) (progn (if (car additional-data) (setf (gethash (caddr additional-data) constants) (gen-token (cadddr additional-data) (cadr additional-data)))
                                                                                      (error "Not yet implemented"))
										  (parse (cdr tokens) region))
                                                   (parse (cdr tokens) region result))))
                                     (otherwise (error (format nil "Invalid region name: ~A" region))))))
		       (t (print cond-stack)
			(case (token-type token)
                            (:region (parse (cdr tokens) (token-value token)))
			    (:number (parsem (parse-number-token token cond-stack)))
			    (:identifier (parsem (parse-identifier-token token constants cond-stack)))
			    (:function  (parsem (parse-function-token token cond-stack)))
			    (otherwise (parse (cdr tokens)))))))))
      (parse tokens)
      (print result))))