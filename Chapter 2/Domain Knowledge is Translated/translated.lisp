;; Includes solution to excercise2-1

(defun Adj* ()
  (if (= (random 2))
      nil
      (append (Adj) (Ajd*))))

(defun PP* ()
  (if (random-elt '(t nil))
      (append (PP) (PP*))
      nil))

(defun sentence ()    (append (noun-phrase) (verb-phrase)))
(defun noun-phrase () (append (append (Article) (Adj*) (Noun) (PP*))))
(defun verb-phrase () (append (Verb) (noun-phrase)))
(defun PP ()          (append (Prep) (noun-phrase)))
(defun Adj ()         (one-of '(big little blue green abiabtic)))
(defun Prep ()        (one-of '(to in by with on)))
(defun Article ()     (one-of '(the a)))
(defun Noun ()        (one-of '(man ball woman table)))
(defun Verb ()        (one-of '(hit took saw liked)))

(defun one-of (set)
  "Pick one element of the set, and make a list of it."
  (list (random-elt set)))

(defun random-elt (choices)
  "Choose an element from a list at random."
  (elt choices (random (length choices))))