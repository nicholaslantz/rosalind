(in-package #:rosalind)

(defun dna (str &optional (stream *standard-output*))
  (format stream "~a ~a ~a ~a"
	  (count #\A str) (count #\C str)
	  (count #\G str) (count #\T str)))

(defun rna (str &optional (stream *standard-output*))
  (format stream "~a" (substitute #\U #\T str)))

(defun revc (str &optional (stream *standard-output*))
  (format stream "~a" (nreverse (dna-complement str))))
