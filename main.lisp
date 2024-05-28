;; Compiles target file and converts it to NyanScript 2.0 binary

;; All token types
;;  '(:number
;;    :identifier
;;    :divider
;;    :function
;;    :user-function
;;    :region
;;    :endline)

;; Data types
;; BYTE - 1 byte number
;; INT - 2 byte number

;; PREREQUISITE LOAD
(eval-when (:compile-toplevel :load-toplevel :execute)
  ;(setf *default-pathname-defaults* #p"C:/Users/kamil/Desktop/NSM/NyanScriptCompiler/")
  (setf *default-pathname-defaults* #p"~/NyanScriptCompiler/")
  (load (compile-file "prerequisites.lisp"))
  (load (compile-file "tokens.lisp"))
  (load (compile-file "type_checking.lisp"))
  (prerequisite-setup))


;;Edit this to add more functions
(defvar *functions-list*
  '(:skip
    :lnum
    :+))

(defun populate-functions-table (list &optional (i 0))
  (if (endp list) nil
    (progn (setf (gethash (car list) *functions-table*) i)
	   (populate-functions-table (cdr list) (1+ i)))))

(defun get-token-end (line j len cond)
  (if (>= j len) j
      (let ((c (aref line j)))
      (cond
       ((or (eql c #\Space) (eql c #\,)) j)
       ((funcall cond c) (get-token-end line (1+ j) len cond))
       (t (error "Compiler error"))))))

(defun handle-line (line len &optional (i 0) skip)
  (if (< i len) (let ((c (aref line i)))
                  (cond
                   ((eql c #\/) (if skip nil (handle-line line len (1+ i) t)))
                   ((eql c #\,) (cons (gen-token "," :divider) (handle-line line len (1+ i))))
                   ((eql c #\:) (build-token :function))
                   ((eql c #\.) (build-token :region))
                   ((digit-char-p c) (build-token :number))
                   ((symbol-char-p c) (build-token :identifier))
                   (t (handle-line line len (1+ i)))))
    (cons (gen-token nil :endline) nil)))
            
(defun tokenize (file)
  (let ((line (read-line file nil)))
    (if line (append (handle-line (string-upcase line) (length line)) (tokenize file))
      nil)))

;TODO ADD TYPE CHECKING WHEN CREATING CONSTANTS AND VARIABLES!

;data parameter format (const type name value)
(defun valid-data-formatp (data)
  (and (cadr data) (caddr data) (cadddr data)))

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

(defun parse-number-token (token output-file)
  (let ((token-value (token-value token)))
    (write-byte (static-typing '(token :lnum :function)) output-file)
    (write-byte (floor token-value 256) output-file)
    (write-byte (mod token-value 256) output-file)))

(defun parse-function-token (token output-file)
  (let ((res (static-typing token)))
    (if res (write-byte res output-file)
	(error (format nil "Unknown function: ~A" (token-value token))))))

(defun parse-identifier-token (token output-file constants)
  (let ((res (gethash (token-value token) constants)))
    (if res (case (gethash (token-type res) *data-types*)
	      (:number (parse-number-token res output-file)))
	(error (format nil "Unknown identifier: ~A" (token-value token))))))

(defun parse-tokens (tokens output-file)
  (let ((constants (make-hash-table)))
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
                       (t (case (token-type token)
                            (:region (parse (cdr tokens) (token-value token)))
			    (:number (parse-number-token token output-file) (parse (cdr tokens)))
			    (:identifier (parse-identifier-token token output-file constants) (parse (cdr tokens)))
			    (:function (parse-function-token token output-file) (parse (cdr tokens)))
			    (otherwise (parse (cdr tokens)))))))))
      (parse tokens))))

(defun compile-script ()
  (populate-functions-table *functions-list*)
  (setf *rslt* nil)
  (format t "Enter filename: ")
  (let* ((fileName (read-line))
         (f (open (format nil "~A.nya" fileName)))
         (fo (open (format nil "~A.bin" fileName) :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))))
    (let ((tokens (tokenize f)))
      (parse-tokens tokens fo)
      (close f)
      (close fo))))
