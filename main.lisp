; Compiles target file and converts it to NyanScript 2.0 binary
(setf *default-pathname-defaults* #p"C:/Users/kamil/Desktop/NSM/NyanScriptCompiler/")

; All token types
;  '(:number
;    :identifier
;    :divider
;    :function
;    :user-function
;    :region
;    :endline))


(defun symbol-char-p (c)
  (and (char>= c #\!) (char<= c #\`)))

(defvar *supported-types* (make-hash-table))
(setf (gethash :number *supported-types*) (list 'parse-integer #'digit-char-p))
(setf (gethash :function *supported-types*) (list 'read-from-string #'symbol-char-p))
(setf (gethash :region *supported-types*) (list 'intern #'alpha-char-p))
(setf (gethash :identifier *supported-types*) (list 'intern #'alpha-char-p))

;Edit this to add more functions
(defvar *functions-list*
  '(:skip
    :lnum
    :+))

(defvar *functions-table* (make-hash-table))

(defun populate-functions-table (list &optional (i 0))
  (if (endp list) nil
    (progn (setf (gethash (car list) *functions-table*) i)
      (populate-functions-table (cdr list) (1+ i)))))

(defun gen-token (string type)
  `(token ,string ,type))

(defun token-value (token)
  (second token))

(defun token-type (token)
  (third token))

(defun regionp (token)
  (eql (token-type token) :region))

(defmacro build-token (type)
  (let ((info (gethash type *supported-types*)))  
    `(let ((token-end (get-token-end line (1+ i) len ,(cadr info))))
       (cons (gen-token (,(car info) (subseq line i token-end)) ,type)
             (handle-line line len token-end)))))

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

(defun handle-identifier (token)
  (case (token-value token)
    (('+) (print "+ ty smejde lol")))
    (identity token))

(defun handle-function (token)
  (let ((function-code (gethash (token-value token) *functions-table*)))
    (labels ((convert-to-binary (n)
               (if (= n 0) ""
                 (let ((code function-code))
                   (setf function-code (floor function-code 2))
                   (format nil "~A~A" (convert-to-binary (1- n)) (mod code 2))))))
      (list (format nil "0b~A" (convert-to-binary 8))))))
                 

(defun handle-number (token)
  (let ((number (token-value token)))
    (labels ((convert-to-binary (n)
               (if (= n 0) ""
                 (let ((num number))
                   (setf number (floor number 2))
                   (format nil "~A~A" (convert-to-binary (1- n)) (mod num 2))))))
      (let ((lower-byte (format nil "0b~A" (convert-to-binary 8)))) 
      (list (format nil "0b~A" (convert-to-binary 8)) lower-byte "0b00000001")))))

(defun convert-to-binary (tokens)
  (if (endp tokens) nil
    (let ((token (car tokens)))
      (append (case (token-type (car tokens))
              ((:function) (handle-function token))
              ((:number) (handle-number token)))
              (convert-to-binary (cdr tokens))))))

;TODO ADD TYPE CHECKING WHEN CREATING CONSTANTS AND VARIABLES!

;data parameter format (const type name value)
(defun valid-data-formatp (data)
  (and (cadr data) (caddr data) (cadddr data)))

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
                            (list constant type name token-value))
                   (list constant type (if (not (eql token-type :identifier)) (error "Invalid data region syntax") token-value)))
          (list constant (if (not (eql token-type :identifier)) (error "Invalid data region syntax") token-value)))))))

(defun parse-tokens (tokens)
  (let ((constants (make-hash-table))
        (testing)
        (result))
    (labels ((parse (tokens &optional (region nil) (additional-data))
               (let ((token (car tokens)))
                 (cond ((null token) nil)
                       (region (if (regionp token) (if (not (eql (token-value token) '.end)) (error (format nil "Unexpected region. Previous region: ~A hasnt been properly closed" region))
                                                       (parse (cdr tokens)))
                                   (case region
                                     ('.data (let ((result (handle-data-region token additional-data)))
                                                (if (eql result :completed) (progn (push (cdr additional-data) testing)
                                                                              (if (car additional-data) (setf (gethash (caddr additional-data) constants) (list (cadr additional-data) (cadddr additional-data)))
                                                                                (error "Not yet implemented"))
                                                                              (parse (cdr tokens) region))
                                                  (parse (cdr tokens) region result))))
                                     (otherwise (error (format nil "Invalid region name: ~A" region))))))
                       (t (case (token-type token)
                            ((:region) (parse (cdr tokens) (token-value token)))))))))
      (parse tokens)
      (print testing))))
                                       
(defun compile-script ()
  (populate-functions-table *functions-list*)
  (format t "Enter filename: ")
  (let* ((fileName (read))
         (f (open (format nil "~A.nya" fileName)))
         (fo (open (format nil "~A.txt" fileName) :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (let ((tokens (tokenize f)))
      (close f)
      (close fo)
      (parse-tokens tokens))))
