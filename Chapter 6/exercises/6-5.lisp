
;; Define a version of compose that allows any number of arguments
;; but is more effecient than the answer to the previous exercise.
;; HINT: try to make decisions when compose is called to build the
;; resulting to function, rather than making the same decision over
;; and over.

(defun compose (&rest functions)
  (case (length functions)
    (0 nil)
    (1 (first functions))
    (2 #'(lambda (x) (funcall (first functions)
			      (funcall (second functions)
				       x))))
    (t #'(lambda (x)
	   (reduce #'funcall functions :from-end t :initial-value x)))))