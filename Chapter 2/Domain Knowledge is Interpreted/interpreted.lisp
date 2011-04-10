(defparameter *simple-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Noun))
    (verb-phrase -> (Verb noun-phrase))
    (Article -> the a)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked))
  "A grammar for a trivial subset of English.")

(defparameter *bigger-grammar*
  '((sentence -> (noun-phrase verb-phrase))
    (noun-phrase -> (Article Adj* Noun PP* (Name) (Pronoun)))
    (verb-phrase -> (Verb noun-phrase PP*))
    (PP* -> () (PP PP*))
    (Adj* -> () (Adj Adj*))
    (PP -> (Prep noun-phrase))
    (Prep -> to in by with on)
    (Adj -> big little blue green adiabtic)
    (Article -> the a)
    (Name -> Pat Kim Lee Terry Robbin)
    (Noun -> man ball woman table)
    (Verb -> hit took saw liked)
    (Pronoun -> he she it these those that))
  "A grammar for a trivial subset of English.")


(defvar *grammar* *bigger-grammar*
  "The grammar used by generate. Initially, this is *bigger-grammar*, but we
   can switch to other grammars.")

(defun rule-lhs (rule)
  "The left-hand side of a rule."
  (first rule))

(defun rule-rhs (rule)
  "The right hand side of a rule."
  (rest (rest rule)))

(defun rewrites (category)
  "Return a list of the possible rewrites for this category."
  (rule-rhs (assoc category *grammar*)))

(defun generate (phrase)
  "Generate a random sentence or phrase."
  (let ((choices (rewrites phrase)))
    (cond ((listp phrase) (mappend #'generate phrase))
	  ((null choices) (list phrase))
	  (t (generate (random-elt choices))))))
	  

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the result."
  (if (null the-list)
      nil
      (append (funcall fn (first the-list)) (mappend fn (rest the-list)))))

(defun generate-tree (phrase)
  "Generate a random sentence or phrase with a complete parse tree."
  (cond ((lisp phrase)
	 (mapcar #'generate-tree phrase))
	((rewrites phrase)
	 (cons phrase
	       (generate-tree (random-elt (rewrites phrase)))))
	(t (list phrase))))