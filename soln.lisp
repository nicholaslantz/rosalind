(in-package #:rosalind)

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
