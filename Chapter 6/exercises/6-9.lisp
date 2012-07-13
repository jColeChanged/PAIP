;; Exercise 6.9 [m]
;; The sorter function is inefficient for two reasons: it calls append,
;; which has to make a copy of the first arugment, and it sorts the entire
;; result, rather than just inserting the new states into the already sorted 
;; old states. Write a more efficient sorter.

(defun sorter (cost-fn)
  (lambda (new old)
    (merge 'list (sort new #'> :key cost-fn) old #'> :key cost-fn)))