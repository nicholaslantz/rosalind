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

(defun dna-complement (str &optional (start 0) end (acc (make-string (length str))))
  (if (or (eql start end) (= start (length str)))
      acc
      (progn
	(setf (aref acc start) (cdr (assoc (aref str start) *dna-complements*)))
	(dna-complement str (1+ start) (length str) acc))))

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

;; FIXME: This function is very inefficient and could be improved if it become an issue
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
      
