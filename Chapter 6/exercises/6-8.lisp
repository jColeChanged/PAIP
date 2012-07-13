(defun tree-search (states goal-p successors combiner)
  ;; Manually dealing with conditions, because I don't know
  ;; how to use the macros to accomplish the same task, but
  ;; do know how to do it without macros.
  (loop
   (if (eq (length states) 0)
     (return nil)
     (if (funcall goal-p (first states))
       (return (first states))
       (setf states (funcall combiner
			     (funcall successors (first states)) 
			     (rest states)))))))

(defun binary-tree (x) (list (* 2 x) (+ 1 (* 2 x))))
(defun is (value) #'(lambda (x) (eql x value)))