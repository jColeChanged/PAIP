;; (let* ((x 6) (y (* x x)))
;;   (+ x y))
;;
;; Show a lambda expression that is equivalent to the above let expression.
;; You may need to use more than one lambda.

(lambda (x) ((lambda (y) (+ x y)) (* x x)))