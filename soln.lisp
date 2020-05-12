(in-package #:rosalind)

(defparameter *data-dir* "/home/anon/projects/rosalind/data/")
  
(defun dna (str &optional (stream *standard-output*))
  (format stream "~a ~a ~a ~a"
	  (count #\A str) (count #\C str)
	  (count #\G str) (count #\T str)))

(defun rna (str &optional (stream *standard-output*))
  (format stream "~a" (substitute #\U #\T str)))

(defun revc (str &optional (stream *standard-output*))
  (format stream "~a" (nreverse (dna-complement str))))

(defun gc (fastas &optional (stream *standard-output*))
  (destructuring-bind (fst . gc)
      (best fastas #'gc-content :key #'fasta-content)
    (format stream "~a~%~F"
	    (fasta-label fst) (coerce (* 100 gc) 'double-float))))

(defun subs (hstk ndl &optional (stream *standard-output*))
  (format stream "~{~d ~}~%" (mapcar #'1+ (all-subseqs ndl hstk))))

(defun grph (ss &optional (stream *standard-output*))
  (let ((graph (overlap-graph ss 3 :key #'fasta-content)))
    (format stream "~{~{~a~^ ~}~^~%~}"
	    (mapcar (lambda (c)
		      (list (fasta-label (car c)) (fasta-label (cdr c))))
		    graph))))

(defun hamm (s1 s2 &optional (stream *standard-output*))
  (format stream "~A~%" (dna-dist s1 s2)))

(defun perm (n &optional (stream *standard-output*))
  (let ((perms (permutations (range 1 (1+ n)))))
    (format stream "~A~%~{~{~A~^ ~}~^~%~}~%" (length perms) perms)))
	    
(defun prot (rna &optional (stream *standard-output*))
  (format stream "~A~%" (funcall (compose #'protein->str #'rna->protein) rna)))

(defun lexf (ab len &optional (stream *standard-output*))
  (let ((words (as~> v (all-words ab len)
		 (mapcar (lambda (w) (mapcar #'symbol-name w)) v)
		 (mapcar (lambda (w) (apply #'concat w)) v)
		 (sort v #'string<))))
    (format stream "~{~A~^~%~}~%" words)))
