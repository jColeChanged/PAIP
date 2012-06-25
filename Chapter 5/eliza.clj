(ns paip.eliza
  (:require [clojure.string :as string])
  (:gen-class))


(defn index-of
  "Returns the index of item. If start is given indexes prior to
  start are skipped."
  ([coll item] (.indexOf coll item))
  ([coll item start]
     (let [unadjusted-index (.indexOf (drop start coll) item)]
       (if (= -1 unadjusted-index)
	 unadjusted-index
	 (+ unadjusted-index start)))))

(defn strings-equal? [x y]
  "Returns true if two strings are equal. Case insensitive."
  (and (string? x) (string? y) (.equalsIgnoreCase x y)))

(defn capture-token? [token]
  "Returns whether the given token captures text."
  (= (first token) \?))

(defn capture-segment-token? [pattern]
  "Returns whether the given token captures segments of text."
  (and (not (string? pattern))
       (string? (first pattern))
       (.startsWith (first pattern) "?*")))
 
(defn bind-capture-token [token input bindings]
  "Associatse token with input in bindings as long as it hasn't been
   associated with a different value."
  (let [input (if (string? input) input (string/join " " input))
	value (get bindings token input)]
    (when (.equalsIgnoreCase value input)
      (assoc bindings token input))))

;; forward declaration
(def segment-match)

(defn match-pattern
  "Matches patterns against input. If matches violate bindings then matching
   has failed. Otherwise returns a map in the form: {'?token' 'match'}."
  ([pattern input] (match-pattern pattern input {}))
  ([pattern input bindings]
     (cond
      ;; If we failed, finished, or the tokens being considered match...
      (or (nil? bindings)
	  (and (empty? pattern) (empty? input)) 
	  (strings-equal? pattern input)) 
        bindings
      ;; If the token being considerd should be saved
      ;; This operates at the rest level when recursing
      (capture-segment-token? pattern)
        (segment-match pattern input bindings)
      ;; This operates at the first level when recursing
      (capture-token? pattern)
        (bind-capture-token pattern input bindings)
      ;; If were not on an end case then compute the next end case and
      ;; treat it as bindings then process everything else... this makes it
      ;; very clear that we are always reducing the work that is left so 
      ;; the function should eventually terminate
      (and (seq? pattern) (seq? input))
        (match-pattern (rest pattern) (rest input)
		       (match-pattern (first pattern) (first input)
				      bindings)))))

(defn segment-match
  ([pattern input bindings] (segment-match pattern input bindings 0))
  ([pattern input bindings start]
     (let [var (first pattern) 
	   pat (rest pattern)]
       (if (empty? pat)
	 (bind-capture-token var input bindings)
	 ;; This code, like Norvig's, assumes that pat starts with a constant
	 (let [pos (index-of input (first pat) start)]
	   (when (not= pos -1)
	     ;; Need to account for position in list despite having trimmed the list...
	     (let [new-bindings (bind-capture-token var (take pos input) bindings)
		   b2 (match-pattern pat (drop pos input) new-bindings)]
	       ;; If the match failed try the next match
	       (if (nil? b2)
		 (segment-match pattern input bindings (inc pos))
		 b2))))))))



(defn tokenize [pattern-string] (string/split pattern-string #" "))
;; A rule is hereby defined to be: (pattern (responses))
(defn get-pattern [rule] (first rule))
(defn get-responses [rule] (second rule))

(def eliza-rules 
  '(("?*x hello ?*y"
     ("How do you do. Please state your problem."))
    ("?*x I want ?*y"
     ("What would it mean if you got ?*y"
      "Suppose you got ?*y soon?"
      "Why do you want ?*y"))
    ("?*x if ?*y"
     ("Do you really think its likely that ?*y")
     ("Do you with that ?*y"))))

(defn substitute-mappings
  [string mappings]
  (let [substitute-mapping 
        (fn [string mapping] 
          (let [pattern (java.util.regex.Pattern/quote (name (key mapping)))
                pattern (re-pattern pattern)
                matcher (re-matcher pattern string) 
                replacement (java.util.regex.Matcher/quoteReplacement (str (val mapping)))]
            (.replaceAll matcher replacement)))]
    (reduce substitute-mapping string (seq mappings))))

(defn use-eliza-rules [input]
  (some #(let [result (match-pattern (tokenize (get-pattern %)) 
				     (tokenize input))]
	   (when result
	     (substitute-mappings (rand-nth (get-responses %)) result)))
	eliza-rules))

(defn eliza []
  (while true
    (print "Eliza> ")
    (flush)
    (println (use-eliza-rules (read-line)))))
    

(defn -main [& args]
  (println "Starting... ") (eliza))