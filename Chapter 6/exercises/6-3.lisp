;; Exercise 6.3 [m]
;; Write a version of ``interactive-interpreter`` that is more general than the
;; one defined in this chapter. Decide what features can be specified, and 
;; provide defaults for them.

(defun interactive-interpreter (&key (read-with #'read) 
				     (eval-with #'eval)
				     (prompt '>)
				     (print-with #'print)
				     (exit-predicate 
				      (lambda (in)
					(equal in "exit"))))
  "Read an expression, transform it, and print the result."
  (loop
   (funcall print-with prompt)
   (let ((input (funcall read-with)))
     (if (funcall exit-predicate input)
       (return)
       (funcall print-with (funcall eval-with input))))))