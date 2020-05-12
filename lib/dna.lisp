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

(let ((complements '((#\A . #\T) (#\T . #\A)
		     (#\C . #\G) (#\G . #\C))))
  (defun dna-complement (str &optional (start 0) end (acc (make-string (length str))))
    (if (or (eql start end) (= start (length str)))
	acc
	(progn
	  (setf (aref acc start) (cdr (assoc (aref str start) complements)))
	  (dna-complement str (1+ start) (length str) acc)))))

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
	    
