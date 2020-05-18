(in-package #:rosalind)

;; TODO: Add constructor that takes strings, build trie out of those
(defclass trie ()
  ((nnodes
    :initform 1
    :reader trie-nnodes)
   (adjacencies
    :reader trie-adjacencies
    :initform (make-array 1
			  :element-type 'list
			  :initial-contents '(())
			  :adjustable t
			  :fill-pointer t))))

(defmethod add-edge ((tr trie) parent data)
  (with-slots (nnodes adjacencies) tr
    (assert (< parent nnodes))
    (if (edgep tr parent data)
	nil
	(progn
	  (push (cons nnodes data) (aref adjacencies parent))
	  (vector-push-extend nil adjacencies)
	  (1- (incf nnodes))))))

(defmethod edgep ((tr trie) parent data)
  (with-slots (adjacencies) tr
    (rassoc data (aref adjacencies parent))))

(defmethod add-string ((tr trie) (str string) &key (parent 0) (start 0) end)
  (if (or (= start (length str)) (and end (= start end)))
      str
      (with-slots (nnodes adjacencies) tr
	(if-let ((entry (edgep tr parent (elt str start))))
	    (add-string tr str :parent (car entry) :start (1+ start))
	    (let ((id (add-edge tr parent (elt str start))))
	      (add-string tr str :parent id :start (1+ start)))))))

(defmethod pattern-match ((str string) (tr trie))
  (with-slots (adjacencies) tr
    
