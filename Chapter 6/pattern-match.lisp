(defconstant fail nil "Indicates pat-match failure")

(defconstant no-bindings '((t . t))
  "Indicates pat-match success, with no variables.")

(defun pat-match (pattern input &optional (bindings no-bindings))
  "Match the pattern against input in the context of the bindings."
  (cond ((eq bindings fail) fail)
	((variable-p pattern)
	 (match-variable pattern input bindings))
	((eql pattern input) bindings)
	((segment-pattern-p pattern)
	 (segment-matcher pattern input bindings))
	((single-pattern-p pattern)
	 (single-matcher pattern input bindings))
	((and (consp pattern) (consp input))
	 (pat-match (rest pattern) (rest input)
		    (pat-match (first pattern) (first input)
			       bindings)))
	(t fail)))

(defun variable-p (x)
  "Is x a variable (a symbol beginning with '?')?"
  (and (symbolp x) (equal (char (symbol-name x) 0) #\?)))

(defun get-binding (var bindings)
  "Find a (variable . value) pair in a binding list."
  (assoc var bindings))

(defun binding-var (binding)
  "Get the variable part of a single binding."
  (car binding))

(defun binding-val (binding)
  "Get the value part of a single binding."
  (cdr binding))

(defun make-binding (var val) (cons var val))

(defun lookup (var bindings)
  "Get the value part (for var) from a binding list."
  (binding-val (get-binding var bindings)))

(defun extend-bindings (var val bindings)
  "Add a (var . value) pair to a binding list."
  (cons (make-binding var val)
	;; Once we add a 'real' binding.
	;; we can get rid of the dummy no-bindings
	(if (eq bindings no-bindings)
	    nil
	    bindings)))

(defun match-variable (var input bindings)
  "Does VAR match input? Uses (or updates) and returns bindings."
  (let ((binding (get-binding var bindings)))
    (cond ((not binding) (extend-bindings var input bindings))
	  ((equal input (binding-val binding)) bindings)
	  (t fail))))


(setf (get '?is 'single-match) 'match-is)
(setf (get '?or 'single-match) 'match-or)
(setf (get '?and 'single-match) 'match-and)
(setf (get '?not 'single-match) 'match-not)
(setf (get '?lit 'single-match) 'match-lit)

(setf (get '?* 'segment-match) 'segment-match)
(setf (get '?+ 'segment-match) 'segment-match+)
(setf (get '?? 'segment-match) 'segment-match?)
(setf (get '?if 'segment-match) 'match-if)

;; This is gluing these functions to the code which calls them...
(defun segment-pattern-p (pattern)
  "Is this a segment-matching pattern like ((?* var) . pat)?"
  (and (consp pattern) (consp (first pattern))
       (symbolp (first (first pattern)))
       (segment-match-fn (first (first pattern)))))

(defun single-pattern-p (pattern)
  "Is this a single-matching pattern?
  E.g. (?is x predicate) (?and . patterns) (?or . patterns)."
  (and (consp pattern)
       (single-match-fn (first pattern))))

(defun segment-matcher (pattern input bindings)
  "Call the right function for this this kind of segment pattern."
  (funcall (segment-match-fn (first (first pattern)))
	   pattern input bindings))

(defun single-matcher (pattern input bindings)
  "Call the right function for this kind of segment pattern."
  (funcall (single-match-fn (first pattern))
	   (rest pattern) input bindings))

(defun segment-match-fn (x)
  "Get the segment match function for x if it is a symbol that
  has one."
  (when (symbolp x) (get x 'segment-match)))

(defun single-match-fn (x)
  "Get the single-match function for x if it is a symbol that has one."
  (when (symbolp x) (get x 'single-match)))

(defun match-is (var-and-pred input bindings)
  "Succeed and bind var if the input satisfies pred
  where var-and-pred is the list (var pred)."
  (let* ((var (first var-and-pred))
	 (pred (second var-and-pred))
	 (new-bindings (pat-match var input bindings)))
    (if (or (eq new-bindings fail)
	    (not (funcall pred input)))
	fail
        new-bindings)))

(defun match-and (patterns input bindings)
  "Succeed if all the patterns match the input"
  (cond ((eq bindings fail) fail)
	((null patterns) bindings)
	(t (match-and (rest patterns) input
		      (pat-match (first patterns) input
				 bindings)))))

(defun match-or (patterns input bindings)
  "Succeed if any one of the patterns match the input."
  (if (null patterns)
      fail
      (let ((new-bindings (pat-match (first patterns)
				     input bindings)))
	(if (eq new-bindings fail)
	    (match-or (rest patterns) input bindings)
	    new-bindings))))

(defun match-lit (pattern input bindings)
  "Matches even when symbols start with '?'"
  (if (eq (first pattern) input)
      bindings
      fail))

(defun match-not (patterns input bindings)
  "Succeed if none of the patterns match the input. This will never bind
  any variables."
  (if (match-or patterns input bindings)
      fail
      bindings))

(defun segment-match (pattern input bindings &optional (start 0))
  "Match the segment pattern ((?* var) . pat) against input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (if (null pat)
	(match-variable var input bindings)
        (let ((pos (first-match-pos (first pat) input start)))
	  (if (null pos)
	      fail
	      (let ((b2 (pat-match
			 pat (subseq input pos)
			 (match-variable var (subseq input 0 pos) bindings))))
		;; If this match failed try another longer one
		(if (eq b2 fail)
		    (segment-match pattern input bindings (+ pos 1))
		    b2)))))))

(defun first-match-pos (pat1 input start)
  "Find the first position that pat1 could possibly match input starting at
  position start. If pat1 is non constant then just return start."
  (cond ((and (atom pat1) (not (variable-p pat1)))
	 (position pat1 input :start start :test #'equal))
	((< start (length input)) start)
	(t nil)))

(defun segment-match+ (pattern input bindings)
  "Match one or more elements of input."
  (segment-math pattern input bindings 1))

(defun segment-match? (pattern input bindings)
  "Match zero or one element of input."
  (let ((var (second (first pattern)))
	(pat (rest pattern)))
    (or (pat-match (cons var pat) input bindings)
	(pat-match pat input bindings))))

(defun match-if (pattern input bindings)
  (and (progv (mapcar #'car bindings)
	      (mapcar #'cdr bindings)
	 (eval (second (first pattern))))
       (pat-match (rest pattern) input bindings)))
  
;; Abbrv facilities... making patterns more readable through transformation
(defun pat-match-abbrev (symbol expansion)
  "Define symbol as a macro standing for a pat-match pattern."
  (setf (get symbol 'expand-pat-match-abbrev)
	(expand-pat-match-abbrev expansion)))

(defun expand-pat-match-abbrev (pat)
  "Expand out all pattern matching abbreviations in pat."
  (cond ((and (symbolp pat) (get pat 'expand-pat-match-abbrev)))
	((atom pat) pat)
	(t (cons (expand-pat-match-abbrev (first pat))
		 (expand-pat-match-abbrev (rest pat))))))
	 