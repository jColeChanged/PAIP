(defun cross-product (cross list-one list-two)
  (mappend #'(lambda (y)
	       (mapcar #'(lambda (x)
			    (funcall cross y x)) ; CL version of apply
			list-two))
	   list-one))

(defun combine-all (list-one list-two)
  (cross-product #'append list-one list-two))