(ns joshua-cole.exercises.three-five
  (:require [clojure.zip :as zip]))

(defn get-answer
  []
  (print "You must answer with either yes or no: ")
  (let [response (read-line)]
    (cond
     (= response "yes") true
     (= response "no") false
     :else (recur))))

