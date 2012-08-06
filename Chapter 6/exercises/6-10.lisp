;; Exercise 6.10 [m] Write a version of graph search and a* search that use
;; hash tables rather than lists to test whether a state has been seen before.

;;;; The Debugging Output Facility:

(defvar *dbg-ids* nil "Identifiers used by dbg")

(defun dbg (id format-string &rest args)
  "Print debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (apply #'format *debug-io* format-string args)))

;; (defun debug (&rest ids)
;;   "Start dbg output on the given ids."
;;   (setf *dbg-ids* (union ids *dbg-ids*)))

;; (defun undebug (&rest ids)
;;   "Stop dbg on the ids.  With no ids, stop dbg altogether."
;;   (setf *dbg-ids* (if (null ids) nil
;;                       (set-difference *dbg-ids* ids))))

;;; ==============================

(defun dbg-indent (id indent format-string &rest args)
  "Print indented debugging info if (DEBUG ID) has been specified."
  (when (member id *dbg-ids*)
    (fresh-line *debug-io*)
    (dotimes (i indent) (princ "  " *debug-io*))
    (apply #'format *debug-io* format-string args)))




;; above was norvig now it is a combo
(defconstant fail nil)

(defun graph-search (states goal-p successors combiner
                     &optional (state= #'eql) 
		               (old-states (make-hash-table :test state=))) 
  "Find a state that satisfies goal-p.  Start with states,
  and search according to successors and combiner.  
  Don't try the same state twice."
  (print states)
  (cond ((null states) fail)
        ((funcall goal-p (first states)) (first states))
        (t (setf (gethash (first states) old-states) (first states))
	   (graph-search
	    (funcall
	     combiner
	     (new-states states successors state= old-states)
	     (rest states))
	    goal-p successors combiner state=
	    old-states))))

(defun new-states (states successors state= old-states)
  "Generate successor states that have not been seen before."
  (remove-if
    #'(lambda (state)
        (or (member state states :test state=)
            (gethash state old-states)))
    (funcall successors (first states))))

(defun next2 (x) (list (+ x 1) (+ x 2)))

(defun prepend (lone ltwo)
  (append ltwo lone))

(defun next2 (x) (list (+ x 1) (+ x 2)))