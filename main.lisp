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
  (prerequisite-setup)
  (load (compile-file "tokens.lisp"))
  (load (compile-file "type_checking.lisp"))
  (load (compile-file "parser.lisp")))

(defun write-to-output (list file)
  (labels ((write-helper (list)
	     (if (endp list) nil
		 (progn (write-byte (car list) file)
			(write-helper (cdr list)))))
	   (write-len (len &optional (n 4))
	     (when (> n 0) (write-len (floor len 256) (1- n)) (write-byte (mod len 256) file))))
    (write-len (length list))
    (write-helper list)))

(defun write-for-testing (list)
  (let ((file (open "test.txt" :direction :output :if-exists :supersede :if-does-not-exist :create)))
    (labels ((write-command (list)
	       (format file "MOV r1, #0x~X~%LSL r1, #8~%ADD r1, #0x~X~%stpush r1, EXEC~%" (first list) (second list)))
	     (write-helper (list)
	       (if (endp list) nil
		   (progn (write-helper (cddr list))
			  (write-command list)))))
      (write-helper list)
      (close file))))

(defun compile-script ()
  (setf *rslt* nil)
  (format t "Enter filename: ")
  (let* ((fileName (read-line))
         (f (open (format nil "~A.nya" fileName)))
         (fo (open (format nil "~A.bin" fileName) :direction :output :if-exists :supersede :if-does-not-exist :create :element-type '(unsigned-byte 8))))
    (let* ((tokens (tokenize f))
	   (parsed-tokens (parse-tokens tokens)))
      (write-to-output parsed-tokens fo)
      (write-for-testing parsed-tokens)
      (close f)
      (close fo))))
