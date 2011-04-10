(defun dot-product (list-one list-two)
  "Computes the dot product of the two lists."
  (if (or (= NIL (first list-one)) (= NIL (first list-two)))
      0
      (+ (* (first list-one) (first list-two))
	 (dot-product (rest list-one) (rest list-two)))))