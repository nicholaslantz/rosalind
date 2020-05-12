(in-package #:rosalind)

(define-test subseqp
  (false (subseqp "12" "345"))
  (false (subseqp "" "hello!"))
  (false (subseqp "123" "12345" :end 2))
  (true (subseqp "123" "12345" :end 3))
  (is = 0 (subseqp "hello" "hello world!"))
  (is = 6 (subseqp "world" "hello world!"))
  (false (subseqp "abc" "123ab"))
  (true (subseqp "AT" "AAT")))

;; FIXME: This function feels more complex than it needs to be.  Revise
(defun subseqp (sseq seq &key (start 0) end (test #'eql) (key #'identity))
  (labels ((forward-check (&optional (count 0))
	     (if (eql count (length sseq))
		 start
		 (let ((a (funcall key (elt sseq count)))
		       (b (funcall key (elt seq (+ start count)))))
		   (if (funcall test a b)
		       (forward-check (1+ count))
		       nil)))))
    (if (or (> (+ start (length sseq)) (length seq))
	    (zerop (length sseq))
	    (and end (> (+ start (length sseq)) end)))
	nil
	(let ((next (position (elt sseq 0) seq)))
	  (if (null next)
	      nil
	      (or (forward-check)
		  (subseqp sseq seq
			   :start (1+ start) :end end
			   :test test :key key)))))))

(defun all-subseqs (sseq seq &key (start 0) end (test #'eql) (key #'identity))
  (loop for s = (subseqp sseq seq :start start :end end :test test :key key)
	  then (subseqp sseq seq :start (1+ s) :end end :test test :key key)
	until (null s)
	collecting s))

;; FIXME: Below could be more efficient
(defun slice-eq (s slice &key (start 0) (test #'eql) (key #'identity))
  (let ((sseq (subseq s start (+ start (length slice)))))
    (every test (map 'list key sseq) (map 'list key slice))))

(defun compose (&rest fs)
  (if (endp (cdr fs))
      (lambda (&rest args) (apply (car fs) args))
      (lambda (&rest args) (funcall (car fs) (apply (apply #'compose (cdr fs)) args)))))