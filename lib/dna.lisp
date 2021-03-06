(in-package #:rosalind)

(defparameter *rna-codon-table*
  '(("UUU" . "F")      ("CUU" . "L")      ("AUU" . "I")      ("GUU" . "V")
    ("UUC" . "F")      ("CUC" . "L")      ("AUC" . "I")      ("GUC" . "V")
    ("UUA" . "L")      ("CUA" . "L")      ("AUA" . "I")      ("GUA" . "V")
    ("UUG" . "L")      ("CUG" . "L")      ("AUG" . "M")      ("GUG" . "V")
    ("UCU" . "S")      ("CCU" . "P")      ("ACU" . "T")      ("GCU" . "A")
    ("UCC" . "S")      ("CCC" . "P")      ("ACC" . "T")      ("GCC" . "A")
    ("UCA" . "S")      ("CCA" . "P")      ("ACA" . "T")      ("GCA" . "A")
    ("UCG" . "S")      ("CCG" . "P")      ("ACG" . "T")      ("GCG" . "A")
    ("UAU" . "Y")      ("CAU" . "H")      ("AAU" . "N")      ("GAU" . "D")
    ("UAC" . "Y")      ("CAC" . "H")      ("AAC" . "N")      ("GAC" . "D")
    ("UAA" . nil)      ("CAA" . "Q")      ("AAA" . "K")      ("GAA" . "E")
    ("UAG" . nil)      ("CAG" . "Q")      ("AAG" . "K")      ("GAG" . "E")
    ("UGU" . "C")      ("CGU" . "R")      ("AGU" . "S")      ("GGU" . "G")
    ("UGC" . "C")      ("CGC" . "R")      ("AGC" . "S")      ("GGC" . "G")
    ("UGA" . nil)      ("CGA" . "R")      ("AGA" . "R")      ("GGA" . "G")
    ("UGG" . "W")      ("CGG" . "R")      ("AGG" . "R")      ("GGG" . "G")))

(defparameter *dna-complements*
  '((#\A . #\T) (#\T . #\A) (#\G . #\C) (#\C . #\G)))
(defparameter *rna-complements*
  '((#\A . #\U) (#\U . #\A) (#\G . #\C) (#\C . #\G)))

(defun dna-complement (nt)
  (assocdr nt *dna-complements*))
(defun rna-complement (nt)
  (assocdr nt *rna-complements*))

(defun codon->amino-acid (codon)
  (assocdr codon *rna-codon-table* :test #'string=))

(defun dna->rna (str)
  (substitute #\U #\T str))

(defun rna->protein (str &key (start 0) stop (protein nil))
  (if (or (>= start (length str)) (and stop (>= start stop)))
      (apply #'concat (nreverse protein))
      (rna->protein str
		    :start (+ start 3) :stop stop
		    :protein (cons (codon->amino-acid (subseq str start (+ start 3))) protein))))

(defun protein->str (prot)
  (apply #'concat (mapcar #'symbol-name (loop for p in prot until (eq p 'stop) collect p))))

(defun dna-str-complement (str &optional (start 0) end (acc (make-string (length str))))
  (if (or (eql start end) (= start (length str)))
      acc
      (progn
	(setf (aref acc start) (cdr (assoc (aref str start) *dna-complements*)))
	(dna-str-complement str (1+ start) (length str) acc))))

(defun gc-content (str)
  (let ((gc-count (count-if (lambda (nt) (or (eq nt #\G) (eq nt #\C))) str)))
    (/ gc-count (length str))))

(defun dna-dist (s1 s2)
  (count-if (lambda (c) (not (eq (car c) (cdr c))))
	    (map 'list #'cons s1 s2)))

(defun splice (str introns)
  (if (null introns)
      str
      (let ((intron (car introns)))
	(splice (apply #'concat (loop
				  for i = 0 then (+ j (length intron))
				  for j = (subseqp intron str :start i)
				  collect (subseq str i j)
				  until (null j)))
		(cdr introns)))))
	    

(defun dna-reverse-palindrome-p (str &key (start 0) (end (1- (length str))) (key #'identity))
  (palindromep str
	       :start start :end end :key key
	       :test (lambda (p q) (eq q (assocdr p *dna-complements* :test #'eq)))))

(defun dna-reverse-palindromes (str n &key (key #'identity))
  (let ((content (funcall key str)))
    (remove-if-not (lambda (start-end)
		     (dna-reverse-palindrome-p content :start (car start-end) :end (cdr start-end)))
		   (mapcar #'cons (range 0 (- (1+ (length str)) n)) (range (1- n) (length str))))))

(defun common-substring-p (ss strs &key (key #'identity))
  (every (lambda (s) (subseqp ss (funcall key s))) strs))

;; FIXME: This function is very inefficient and could be improved if it becomes an issue
;;; Notably: This function tries every consecutive string of length test-length.
;;;          Instead, it could find every short common substring and attempt to expand those
;;;          until a longest one is found.  This would be very beneficial in strings where
;;;          there is not much they have in common.
(defun longest-common-substring (strs &key generator (test-length (length generator)) (key #'identity))
  (when (null generator)
    (setf generator (car (best strs #'length :predicate #'< :key key)))
    (setf test-length (length generator)))
  (if (zerop test-length)
      nil
      (let ((res
	      (find-if (lambda (start-end)
			 (let ((start (car start-end))
			       (end (cdr start-end)))
			   (common-substring-p (subseq generator start end) strs)))
		       (mapcar #'cons
			       (range 0 (- (1+ (length generator)) test-length))
			       (range (1- test-length) (length generator))))))
	(if res
	    (subseq generator (car res) (cdr res))
	    (longest-common-substring
	     strs :generator generator :test-length (1- test-length) :key key)))))
      
;; FIXME: :key may not work as intended in CONSENSUS and PROFILE below
(defun consensus (strs &key (test #'eql) (syms '(#\A #\T #\G #\C)) (key #'identity))
  (assert (apply #'= (mapcar (compose #'length key) strs))
	  (strs)
	  "Cannot reach consensus with strings of unequal length ~A" strs)
  (mapcar (lambda (ind)
	    (let ((chars
		    (coerce (map 'list (lambda (str) (elt str ind)) strs)
			    'string)))
	      (mapcar (lambda (sym) (cons sym (count sym chars :test test))) syms)))
	  (range (length (car strs)))))

(defun profile (strs &key (test #'eql) (syms '(#\A #\T #\G #\C)) (key #'identity))
  (let ((c (consensus strs :test test :syms syms :key key)))
    (coerce (mapcar (lambda (syms)
		      (caar (best syms #'identity :key #'cdr)))
		    c)
	    'string)))

(defun kmer-composition (str k &optional (key #'identity))
  (let ((comp (make-array (expt 4 k) :initial-element 0)))
    (let ((str (funcall key str)))
      (dotimes (i (1+ (- (length str) k)) comp)
	(incf (aref comp (lexicographic-order str :start i :end (+ i k))))))))

(defparameter *possible-rna-bindings-table* nil)
(defun possible-rna-bindings (str &key (start 0) (end (length str)) (refresh-table t))
  (when refresh-table
    (setf *possible-rna-bindings-table* '(("" . 1) ("AU" . 1) ("UA" . 1) ("CG" . 1) ("GC" . 1)))
    (assert (= (count #\U str) (count #\A str)))
    (assert (= (count #\G str) (count #\C str)))
    (assert (evenp (- end start))))
  (if-let ((entry (assoc (subseq str start end) *possible-rna-bindings-table* :test #'string=)))
    (cdr entry)
    (let ((res
	    (apply #'+ (mapcar (lambda (p)
				 (* (possible-rna-bindings str
							   :start (1+ start) :end p
							   :refresh-table nil)
				    (possible-rna-bindings str
							   :start (1+ p) :end end
							   :refresh-table nil)))
			       (let ((comp (rna-complement (elt str start))))
				 (remove-if-not (lambda (ind) (eq comp (elt str ind)))
						(range (1+ start) end 2)))))))
      (push (cons (subseq str start end) res) *possible-rna-bindings-table*)
      res)))

(defparameter *longest-common-subsequence-table* nil)
(defun longest-common-subsequence (s1 s2 &key
					   (s1-start 0) (s2-start 0)
					   (refresh-table t))
  (when refresh-table (setf *longest-common-subsequence-table* nil))
  (if-let ((entry (assoc (cons s1-start s2-start)
			 *longest-common-subsequence-table*
			 :test #'equal)))
    (cdr entry)
    (if (or (>= s1-start (length s1)) (>= s2-start (length s2)))
	nil
	(let ((res (if (eq (elt s1 s1-start) (elt s2 s2-start))
		       (cons (elt s1 s1-start)
			     (longest-common-subsequence
			      s1 s2
			      :s1-start (1+ s1-start) :s2-start (1+ s2-start)
			      :refresh-table nil))
		       (let ((a (longest-common-subsequence
				 s1 s2
				 :s1-start (1+ s1-start) :s2-start (1+ s2-start)
				 :refresh-table nil))
			     (b (longest-common-subsequence
				 s1 s2
				 :s1-start s1-start :s2-start (1+ s2-start)
				 :refresh-table nil))
			     (c (longest-common-subsequence
				 s1 s2
				 :s1-start (1+ s1-start) :s2-start (1+ s2-start)
				 :refresh-table nil)))
			 (car (best (list a b c) #'length))))))
	  (push (cons (cons s1-start s2-start) res) *longest-common-subsequence-table*)
	  (format t "~A~%" *longest-common-subsequence-table*)
	  res))))
			     
				
