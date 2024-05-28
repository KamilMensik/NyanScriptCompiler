(defun gen-token (string type)
  `(token ,string ,type))

(defun token-value (token)
  (second token))

(defun token-type (token)
  (third token))

(defun regionp (token)
  (eql (token-type token) :region))

(defmacro build-token (type)
  (labels ((get-info-for-build-token (type)
	     (gethash type *supported-types*)))
    (let ((info (get-info-for-build-token type)))  
      `(let ((token-end (get-token-end line (1+ i) len #',(cadr info))))
	 (cons (gen-token (,(car info) (subseq line i token-end)) ,type)
	       (handle-line line len token-end))))))
