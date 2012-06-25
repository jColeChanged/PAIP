;; Write a function that will print an expression in dotted pair notation.
;; Use the built-in princ function to print each component of the expression.

(defun dotted-pair-print (expression)
  "Print out expression in dotted pair notation."
  (when (and (not (listp (second expression))) (= (length expression) 2))
    (princ "(")
    (princ (first expression))
    (princ " . ")
    (princ (second expression))
    (princ ")")
    NIL))