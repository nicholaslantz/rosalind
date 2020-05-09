(in-package #:rosalind)

(defclass fasta ()
  ((label
    :initarg :label
    :accessor fasta-label
    :initform (error "Must provide label for fasta"))
   (content
    :initarg :content
    :accessor fasta-content
    :initform (error "Must provide content for fasta"))))

(defun read-fasta (&optional (stream *standard-input*))
  (let ((label (read-line stream))
	(content nil))
    (loop
      (when (eq (peek-char nil stream nil #\> nil) #\>)
	(return (make-instance 'fasta
			       :label (subseq label 1)
			       :content (apply #'concat (nreverse content)))))
      (push (read-line stream) content))))

(defun read-fastas (&optional (stream *standard-input*))
  (loop for fst = (read-fasta stream)
	collecting fst
	until (eq (peek-char nil stream nil :eof) :eof)))

(defmethod write-object ((fst fasta) stream)
  (format stream "#<FASTA :LABEL ~a>" (fasta-label fst)))
