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
    (every test (map 'vector key sseq) (map 'vector key slice))))

(defun offsets-eq (s1 s2 count
		   &key
		     (s1-start 0) (s2-start 0)
		     (test #'eql) (key #'identity))
  (if (zerop count)
      t
      (let ((a (funcall key (elt s1 s1-start)))
	    (b (funcall key (elt s2 s2-start))))
	(if (not (funcall test a b))
	    nil
	    (offsets-eq s1 s2 (1- count)
			:s1-start (1+ s1-start) :s2-start (1+ s2-start)
			:test test :key key)))))

(defun compose (&rest fs)
  (if (endp fs)
      (lambda (arg) arg)
      (lambda (arg)
	(funcall (car fs)
		 (funcall (apply #'compose (cdr fs)) arg)))))

(defun permutations (lst)
  (let ((res nil))
    (dolist (p lst res)
      (let ((next (permutations (remove p lst))))
	(if (null next)
	    (push (list p) res)
	    (dolist (q next)
	      (push (cons p q) res)))))))

(defun all-words (ab len)
  (cond ((zerop len) nil)
	((= 1 len) (mapcar #'list ab))
	(t (let ((res nil))
	     (dolist (sym ab res)
	       (let ((next (all-words ab (1- len))))
		 (setf res (append (mapcar (lambda (l) (cons sym l))
					   next)
				   res))))))))

(defun read-all-from-string (str &key (start 0) end (acc nil))
  (if (or (>= start (length str)) (and end (>= start end)))
      (nreverse acc)
      (multiple-value-bind (val len)
	  (read-from-string str nil :eof :start start)
	(read-all-from-string str :start len :end end :acc (cons val acc)))))
  
