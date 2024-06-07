(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *rslt* '())
  (defvar *if-branch* '())
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

(setf (getfunh :end) (fun-token 0 '() '()))
(setf (getfunh :lnum) (fun-token 1 '() '(:number)))
(setf (getfunh :+) (list (fun-token 2 '(:number :number) '(:number))))
(setf (getfunh :print) (fun-token 3 '() '()))
(setf (getfunh :if) (fun-token 4 '(:number) '()))
(setf (getfunh :else) (fun-token 5 '() '()))
(setf (getfunh :then) :ignore)


(defun context-push (el)
  (cond ((null *if-branch*) (push el *rslt*))
        ((null (first (car *if-branch*))) (push el (second (car *if-branch*))))
        (t (push el (third (car *if-branch*))))))

(defun context-pop ()
  (cond ((null *if-branch*) (pop *rslt*))
        ((null (first (car *if-branch*))) (pop (second (car *if-branch*))))
        (t (pop (third (car *if-branch*))))))

(defun pushout (out)
  (if (endp out) nil
      (progn (context-push (car out))
	     (pushout (cdr out)))))

(defun empty-if-branch-el ()
  (list nil '() '()))

(defun static-typing (token)
  (let ((params))
    (print *if-branch*)
    (labels ((check-single (inputs params)
	       (if (endp inputs) t
		   (and (eql (car inputs) (car params)) (check-single (cdr inputs) (cdr params)))))
	     (check-multiple (functions)
	       (if (endp functions) nil
		   (if (check-single (cdr (fun-in (car functions))) params) (car functions)
		       (check-multiple (cdr functions))))))
      (let ((function (gethash (token-value token) *functions*)))
        (prog1
            (cond ((null function) (error (format nil "Function ~A not found" (token-value token))))
                  ((eql function :ignore) :ignore)
                  (t (dotimes (i (car (fun-in (if (atom (car function)) function (car function)))))
                       (let ((in (context-pop)))
                         (if in (push in params)
                           (error (format nil "Invalid parameter count for function ~A" (token-value token))))))
                     (if (atom (car function)) (if (check-single (cdr (fun-in function)) params) (prog1 (fun-code function) (pushout (fun-out function)))
                                                 (error (format nil "Invalid param format for function ~A" (token-value token))))
                       (let ((res (check-multiple function)))
                         (if res (prog1 (fun-code res) (pushout (fun-out res)))
			   (error (format nil "Invalid param format for function ~A" (token-value token))))))))
          (case (token-value token)
            (:if (push (empty-if-branch-el) *if-branch*))
            (:else (cond ((null *if-branch*) (error "If branch is empty!"))
                         ((first (car *if-branch*)) (error "Two else methods detected in one if branch"))
                         (t (setf (first (car *if-branch*)) t))))
            (:then (cond ((null *if-branch*) (error "If branch is empty!"))
                         ((null (first (car *if-branch*))) (error "Missing else branch!"))
                         (t (let ((if-branch (pop *if-branch*)))
                              (if (equal (second if-branch) (third if-branch)) (pushout (second if-branch))
                                (error "Two branches dont have identical output types"))))))))))))
    
