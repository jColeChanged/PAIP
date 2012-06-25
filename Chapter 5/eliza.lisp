;; Eliza was a chat bot built in the 1960s. It used pattern matching to trick
;; those who interacted with it into thinking that it had some measure of
;; intelligence. According to Peter Norvig in PAIP, Eliza was not a serious
;; an attempt at a serious AI. Knowing how the program works helps to explain
;; what he means by that.

;; The Eliza chat bot works by first reading input, then finding a pattern which
;; matches the input. Using the pattern as a guide the bot proceeds to transform
;; the input into a response which is then printed.


;; Demonstrates the similarity between the common lisp function equal and the
;; code used for pattern matching.
(defun starts-with (list x)
  "Is this a list whose first element is x?"
  (and (consp list) (eql (first list) x)))

(defun simple-equal (x y)
  "Are x and y equal? (Don't check inside strings.)"
  (if (or (atom x) (atom y))
      (eql x y)
      (and (simple-equal (first x) (first y))
	   (simple-equal (rest x) (rest y)))))

;; This is to make sure that the variable to replace in a pattern is handled
;; when testing patterns.
(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

;; Being used to make pat-match, a function defined below, be a true predicate
;; in that it only returns nil when there isn't a match.
(defconstant fail nil "Indicates pat-match failure.")

(defconstant no-bindings '((t . t)) 
  "Indicates pat-match success with no vars.")

(defun get-binding (var bindings)
  "Finds a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a bindings list."
  (cons (cons var val) bindings))

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match a pattern against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-match pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input) 
			       bindings)))
	(t fail)))
	

(defun segment-pattern-p (pattern)
  "Is this a segment matching pattern: ((?* var) . pat)"
  (and (consp pattern)
       (starts-with (first pattern) '?*)))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
        ;; We asssume that pat starts with a constant
        ;; In other words, a pattern can't have 2 consecutive vars
        (let ((pos (position (first pat) input 
			     :start start :test #'equal)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match 
			 pat (subseq input pos)
			 (match-variable var (subseq input 0 pos)
					 bindings))))
		;; If the match failed try another longer one
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun match-variable (var input bindings)
  "Does var match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))

;; So far the Eliza program doesn't seem to be doing anything complicated or
;; advanced. So far it just works by capturing a little tiny bit about what
;; someone is saying through pattern matching and then based on the pattern
;; which was matched it proceeds to output something. The thing it outputs
;; can also take a bit of captured text and return it. To me, it seems very
;; similar to a glorified regular expression.

;; In fact, as I look at what Norvig is having me write out, I notice that
;; it reminds me a lot of a regular expression matching program.

;; Here is Eliza:

(defun rule-pattern (rule) (first rule))
(defun rule-responses (rule) (rest rule))

(defparameter *eliza-rules*
  '((((?* ?X) hello (?* ?y))
      (How do you do. Please state your problem.))
    (((?* ?x) I want (?* ?y))
     (What would it mean if you got ?y)
     (Why do you want ?y)
     (Suppose you got ?y soon))
    (((?* ?x) if (?* ?y))
     (Do you really thinks its likely that ?y)
     (Do you wish that ?y)
     (What would you think about ?y)
     (Really-- if ?y))
    (((?* ?x) no (?* ?y))
     (Why not?)
     (You are being a bit negative)
     (Are you saying "NO" just to be negative?))
    (((?* ?x) I was (?* ?y))
     (Were you really?)
     (Perhaps I already knew you were ?y)
     (Why do you tell me you were ?y now?))
    (((?* ?x) I feel (?* ?y))
     (Do you often feel ?y ?))
    (((?* ?x) I felt (?* ?y))
     (What other feelings do you have?))))

(defun eliza ()
  "Responds to user input using patter matching rules."
  (loop
   (print 'eliza>)
   (let ((in (read)))
     (if (equal in '(exit))
       (exit)
       (write (flatten (use-eliza-rules in)) :pretty t)))))

(defun use-eliza-rules (input)
  "Find some rules with which to transform the input."
  (some #'(lambda (rule)
	    (let ((result (pat-match (rule-pattern rule) input)))
	      (if (not (eq result fail))
		  (sublis (switch-viewpoint result)
			  (random-elt (rule-responses rule))))))
	*eliza-rules*))

(defun switch-viewpoint (words)
  "Change I to you and vice versa and so on"
  (sublis '((I . you) (you . I) (me . you) (am . are)) words))

(defun flatten (the-list)
  "Append together elements (or lists) in the list."
  (mappend #'mklist the-list))

(defun mklist (x)
  "Return x if it is a list. otherwise (x)."
  (if (listp x)
      x
      (list x)))

(defun mappend (fn the-list)
  "Apply fn to each element of list and append the result."
  (apply #'append (mapcar fn the-list)))

(defun random-elt (choices)
  "Chooses an element from a list at random."
  (elt choices (random (length choices))))