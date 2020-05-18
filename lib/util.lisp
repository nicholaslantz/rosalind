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
  

;; XXX: A /subsequence/ is a sequence in which its elements share the same ordering as
;;      a /supersequence/.  For instance, '(2 3 4) is a subsequence of '(1 2 5 6 3 0 4), but
;;      '(2 6 5) is not.
(defun subsequence (sseq seq &key (start 0) end (inds (make-array (length sseq) :fill-pointer 0)))
  (cond ((= (length inds) (length sseq)) inds)
	((or (eq start (length seq)) (and end (>= start end))) nil)
	(t (let ((pos (position (elt sseq (length inds)) seq :start start :end end)))
	     (when pos
	       (vector-push pos inds)
	       (subsequence sseq seq :start (1+ pos) :end end :inds inds))))))

(define-test palindromep
  (true (palindromep "racecar"))
  (true (palindromep '(1 2 3 3 2 1)))
  (true (palindromep "ATGTG" :start 1 :end 3))
  (false (palindromep "ATGTG")))

(defun palindromep (seq &key (start 0) (end (1- (length seq))) (test #'eql) (key #'identity))
  (cond ((>= start end) t)
	((not (funcall test
		       (funcall key (elt seq start))
		       (funcall key (elt seq end))))
	 nil)
	(t (palindromep seq :start (1+ start) :end (1- end) :test test :key key))))

(defun factorial (n)
  (assert (>= n 0))
  (%factorial n 1))

(defun %factorial (n acc)
  (if (= 0 n)
      acc
      (%factorial (1- n) (* n acc))))

(defun bin-coeff (n k)
  (assert (>= n k))
  (/ (factorial n)
     (* (factorial k) (factorial (- n k)))))

(defun xor (a b)
  (or (and (not a) b) (and a (not b))))

(defun make-ring (list)
  (rplacd (last list) list)
  list)

(define-test subring-equal
  (let ((a (make-ring (list 1 2 3 4 5)))
	(b (make-ring (list 1 2 1 2))))
    (false (subring-equal (cons a (cddr a)) (cons (cddr a) (nthcdr 4 a))))
    (true  (subring-equal (cons a a) (cons a a)))
    (true  (subring-equal (cons a (cdr a)) (cons b (cdr b))))))

(defun subring-equal (p q &key (key #'identity) (test #'eql))
  (destructuring-bind ((s1 . e1) (s2 . e2))
      (list p q)
    (let ((a (funcall key (car s1))) (b (funcall key (car s2))))
      (if (not (funcall test a b))
	  nil
	  (cond ((xor (eq s1 e1) (eq s2 e2)) nil)
		((and (eq s1 e1) (eq s2 e2)) t)
		(t (subring-equal (cons (cdr s1) e1) (cons (cdr s2) e2)
				  :key key :test test)))))))

(defun ring-last (ring)
  (%ring-last ring (car ring)))

(defun %ring-last (ring head)
  (if (eq (cadr ring) head)
      ring
      (%ring-last (cdr ring) head)))

(defun lexicographic-order (str &key (start 0) end (ab "ACGT") (acc 0))
  (if (or (= start (length str)) (and end (= start end)))
      acc
      (let* ((len (if end (- end start) (- (length str) start)))
	     (size (expt (length ab) (1- len))))
	(lexicographic-order str
			     :start (1+ start) :end end :ab ab
			     :acc (+ acc (* size (position (elt str start) ab)))))))
    
