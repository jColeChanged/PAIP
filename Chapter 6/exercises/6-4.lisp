;; Exercise 6.4
;; Write a  version of compoase which takes any number of arguments.

(defun enclose (f g)
  "Wraps g in f."
  #'(lambda (x)
      (funcall f (funcall g x))))

(defun compose (&rest functions)
  "Takes any number of functions and returns a lambda: (f1 (f2 .. (fn args)))."
  (reduce #'enclose functions))

;; This function has the limitation that it doesn't support zero arguments.
;; However, it also doesn't support a negative number of arguments. nil or
;; an error are among the most reasonable responses to calling compose with no
;; arguments.

;; My approach was to repeatedly wrap the functions I pass in with closurse.
;; Norvig took a very different approach, one which I considered, but found
;; difficult to implement, because I wasn't sure how to specify a default value
;; and as I lacked an internet connection I wasn't able to look up the syntax.

;; Here is his approach:
;; (defun compose (&rest functions)
;;   "Return the function that is the composition of all the args.
;;   i.e. (compose f g h) = (lambda (x) (f (g (h x))))."
;;   #'(lambda (x)
;;       (reduce #'funcall functions :from-end t :initial-value x)))