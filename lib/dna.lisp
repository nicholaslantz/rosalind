(in-package #:rosalind)

(let ((complements '((#\A . #\T) (#\T . #\A)
		     (#\C . #\G) (#\G . #\C))))
  (defun dna-complement (str &optional (start 0) end (acc (make-string (length str))))
    (if (or (eql start end) (= start (length str)))
	acc
	(progn
	  (setf (aref acc start) (cdr (assoc (aref str start) complements)))
	  (dna-complement str (1+ start) (length str) acc)))))

(defun gc-content (str)
  (let ((gc-count (count-if (lambda (nt) (or (eq nt #\G) (eq nt #\C))) str)))
    (/ gc-count (length str))))
		     
    
