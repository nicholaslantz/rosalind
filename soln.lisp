(in-package #:rosalind)

(defparameter *data-dir* "/home/anon/projects/rosalind/data/")
  
(defun dna (str &optional (stream *standard-output*))
  (format stream "~a ~a ~a ~a"
	  (count #\A str) (count #\C str)
	  (count #\G str) (count #\T str)))

(defun rna (str &optional (stream *standard-output*))
  (format stream "~a" (substitute #\U #\T str)))

(defun revc (str &optional (stream *standard-output*))
  (format stream "~a" (nreverse (dna-str-complement str))))

(defun gc (fastas &optional (stream *standard-output*))
  (destructuring-bind (fst . gc)
      (best fastas #'gc-content :key #'fasta-content)
    (format stream "~a~%~F"
	    (fasta-label fst) (coerce (* 100 gc) 'double-float))))

(defun subs (hstk ndl &optional (stream *standard-output*))
  (format stream "~{~d ~}~%" (mapcar #'1+ (all-subseqs ndl hstk))))

;; FIXME: Overlap-graph seems to have disappeared...
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

(defun splc (str introns &optional (stream *standard-output*))
  (format stream "~A~%"
	  (~>> (splice str introns)
	    (dna->rna)
	    (rna->protein))))

(defun mrna (str &optional (stream *standard-output*))
  (let ((n (apply #'*
		  (append '(3)
			  (map 'list
			       (lambda (p)
				 (count-if (lambda (c) (and (cdr c) (eq (elt (cdr c) 0) p))) *rna-codon-table*))
			       str)))))
    (multiple-value-bind (f res)
	(floor n 1000000)
      (declare (ignore f))
      (format stream "~A~%" res))))
			      
(defun problem-sseq (sseq seq &optional (stream *standard-output*))
  (format stream "~{~A~^ ~}~%" (mapcar #'1+ (coerce (subsequence (fasta-content sseq)
								 (fasta-content seq))
						    'list))))

(defun revp (str &optional (stream *standard-output*))
  (let ((results (append (dna-reverse-palindromes str 4)
			 (dna-reverse-palindromes str 6)
			 (dna-reverse-palindromes str 8)
			 (dna-reverse-palindromes str 10)
			 (dna-reverse-palindromes str 12))))
    (format stream "~{~{~A~^ ~}~^~%~}~%"
	    (sort (mapcar (lambda (start end)
			    (list (1+ start) (1+ (- end start))))
			  (mapcar #'car results)
			  (mapcar #'cdr results))
		  (lambda (a b) (< (car a) (car b)))))))

(defun lcsm (strs &optional (stream *standard-output*))
  (format stream "~A~%" (longest-common-substring strs)))

(defun fib (n k)
  (cond ((= n 0) 0)
	((= n 1) 1)
	((= n 2) 1)
	(t (+ (* k (fib (- n 2) k))
	      (fib (- n 1) k)))))

(defun problem-cons (strs &optional (stream *standard-output*))
  (format stream "~A~%" (profile strs))
  (let ((c (consensus strs)))
    (let ((syms (mapcar #'car (car c)))
	  (counts (apply #'zip (mapcar (lambda (cell) (mapcar #'cdr cell)) c))))
      (dolist (sym-count (sort (mapcar #'cons syms counts)
			       (lambda (a b) (char< (car a) (car b)))))
	(format stream "~A: ~{~A~^ ~}~%" (car sym-count) (cdr sym-count))))))

(defun pmch (str &optional (stream *standard-output*))
  (let ((nau (count #\A str))
	(ncg (count #\C str)))
    (format stream "~A~%"
	    (* (factorial nau) (factorial ncg)))))

(defun cat (str &optional (stream *standard-output*))
  (format stream "~A~%" (mod (possible-rna-bindings str) 1000000)))

(defun pper (n k &optional (stream *standard-output*))
  (format stream "~A~%" (mod (* (bin-coeff n k) (factorial k)) 1000000)))

(defun kmer (str &optional (stream *standard-output*))
  (format stream "~{~A~^ ~}~%" (coerce (kmer-composition str 4) 'list)))

(defun iprb (k m n &optional (stream *standard-output*))
  ;; All probabilities below are the probability that the 2 kinds of
  ;; given organism produce offspring without a dominant allele.
  (let ((p-het-het (/ 1 4)) (p-het-rec (/ 1 2)) (p-rec-rec 1))
    (let* ((total-pairings (bin-coeff (+ k m n) 2))
	   (het-het-pairings (bin-coeff m 2))
	   (rec-rec-pairings (bin-coeff n 2))
	   (het-rec-pairings (- (bin-coeff (+ m n) 2) het-het-pairings rec-rec-pairings)))
      (let ((res (- 1 (/ (+ (* p-het-het het-het-pairings)
			    (* p-het-rec het-rec-pairings)
			    (* p-rec-rec rec-rec-pairings))
			 total-pairings))))
	(format stream "~,5F~%" (coerce res 'double-float))))))

;; TODO: Move to another file
(defun prob-string (str gc &key (start 0) (acc 1.d0))
  (if (= start (length str))
      acc
      (let ((c (elt str start)))
	(if (member c '(#\C #\G))
	    (prob-string str gc :start (1+ start) :acc (* acc (/ gc 2)))
	    (prob-string str gc :start (1+ start) :acc (* acc (/ (- 1.0 gc) 2)))))))
  
(defun prob (str b &optional (stream *standard-output*))
  (let ((res (mapcar (lambda (gc) (log (prob-string str gc) 10))
		     b)))
    (format stream "~{~,3F~^ ~}~%" res)))
  
(defun tree (n edges &optional (stream *standard-output*))
  (format stream "~A~%" (- n 1 (length edges))))

(defun sset (n &optional (stream *standard-output*))
  (let ((c (apply #'+ (mapcar (lambda (k) (bin-coeff n k)) (range (1+ n)))))
	;; It turns out the above is just 2^n,
	;; Look at [[https://en.wikipedia.org/wiki/Power_set#Properties]] for more details
	(better-c (expt 2 n)))
    (declare (ignore better-c))
    (format stream "~A~%" (mod c 1000000))))

(defun inod (n &optional (stream *standard-output*))
  (format stream "~A~%" (- n 2)))
	     
