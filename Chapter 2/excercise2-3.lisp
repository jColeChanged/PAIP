;; This program will frequently throw a StackOverflow Error, since there is no
;; guarantee that it will not recurse infinitely.

(defparameter *simple-grammar*
  '((equation -> (Operand+ Operand Operator))
    (Operand+ -> (Operand) (Operand Operand*))
    (Operand* -> () (Operand Operand*))
    (Operand  -> (equation) (Number))
    (Number   -> 1 2 3 4 5 6 7 8 9 10)
    (Operator -> + - * /))
  "Generates a postfix math notation")

(defvar *grammar* *simple-grammar*
  "Grammar used throughout the program. Starts out as *simple-grammar*, but this
   could change.")

 (defparameter *simple-grammar*
  '((equation -> (Operand* Operand Operand Operator))
    (Operand* -> () (Operand Operand*))
    (Operand  -> (equation) (Number))
    (Number   -> 1 2 3 4 5 6 7 8 9 10)
    (Operator -> + - * /))
  "Generates a postfix math notation")

(defvar *grammar* *simple-grammar*
  "Grammar used throughout the program. Starts out as *simple-grammar*, but this
   could change.")

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