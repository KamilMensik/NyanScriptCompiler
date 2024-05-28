(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *rslt* '())
  (defvar *functions* (make-hash-table)))

(defmacro getfunh (key)
  `(gethash ,key *functions*))

(defun fun-token (n in out)
  (list n (cons (length in) in) out))

(defun fun-code (fun)
  (first fun))

(defun fun-in (fun)
  (second fun))

(defun fun-out (fun)
  (third fun))

(setf (getfunh :skip) (fun-token 0 '() '()))
(setf (getfunh :lnum) (fun-token 1 '() '(:number)))
(setf (getfunh :+) (list (fun-token 2 '(:number :number) '(:number))))

(defun pushout (out)
  (if (endp out) nil
      (progn (push (car out) *rslt*)
	     (pushout (cdr out)))))

(defun static-typing (token)
  (let ((params))
    (labels ((check-single (inputs params)
	       (if (endp inputs) t
		   (and (eql (car inputs) (car params)) (check-single (cdr inputs) (cdr params)))))
	     (check-multiple (functions)
	       (if (endp functions) nil
		   (if (check-single (cdr (fun-in (car functions))) params) (car functions)
		       (check-multiple (cdr functions))))))
      (let ((function (gethash (token-value token) *functions*)))
	(when (null function) (error (format nil "Function ~A not found" (token-value token))))
	(dotimes (i (car (fun-in (if (atom (car function)) function (car function)))))
	  (let ((in (pop *rslt*)))
	    (if in (push in params)
		(error (format nil "Invalid parameter count for function ~A" (token-value token))))))
	(if (atom (car function)) (if (check-single (cdr (fun-in function)) params) (prog1 (fun-code function) (pushout (fun-out function)))
				      (error (format nil "Invalid param format for function ~A" (token-value token))))
	    (let ((res (check-multiple function)))
	      (if res (prog1 (fun-code res) (pushout (fun-out res)))
		  (error (format nil "Invalid param format for function ~A" (token-value token))))))))))
  
