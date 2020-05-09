(in-package #:rosalind)

(defun dna (str &optional (stream *standard-output*))
  (format stream "~a ~a ~a ~a"
	  (count #\A str) (count #\C str)
	  (count #\G str) (count #\T str)))
