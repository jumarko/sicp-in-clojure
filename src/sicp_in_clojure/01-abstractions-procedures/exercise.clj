(ns sicp-in-clojure.01-abstractions-procedures.exercise
  "All 46 exercises from the Chapter 1.")

;;; Ex. 1.2 (p.21)
(/ (+ 5
      4
      (- 2
         (- 3 (+ 6 4/5))))
   (* 3
      (- 6 2)
      (- 2 7)))
;;=> -37/150
;; this is the result from interactive sicp: https://github.com/IIIIllllIIIIllllIIIIllllIIIIllllIIIIll/isicp/blob/master/content/1-1-elements.content.html
(/ (+ 5 4 (- 2 (- 3 (+ 6 (/ 4 5))))) (* 3 (- 6 2) (- 2 7)))


;;; Ex. 1.3 (p.21)
(defn drop-min-and-sum
  "Takes 3 numbers as arguments and returns the sum of the two larger numbers"
  [x y z]
  (->> [x y z]
       sort
       rest
       (apply +)))
(drop-min-and-sum 10 2 18)
(drop-min-and-sum -10 0 1)
(drop-min-and-sum -1 -2 -100)


;;; Ex. 1.4 (p.21)
;;; Observer that our model of evaluation allows for combinations whose operators
;;; are compound experssions. How would you describe the behavior of the following?
(defn a-plus-abs-b [a b]
  ((if (> b 0) + -)
   a
   b))
;; Adds a to b (if b is positive)
(a-plus-abs-b 10 2)
;; or subtracts b from a (if b is negative)
(a-plus-abs-b 10 0)
(a-plus-abs-b 10 -2)
;; => that is it always adds absolute value of b to a


;;; Ex. 1.5 (p.21)
;;; Ben Bitdiddle's test for order evaluation (applicative vs normal)
(defn p [] (p))
(defn test [x y]
  (if (= x 0)
    0
    y))
;; (test 0 (p))
;;=> stackoverflow (aka infinite recursion) because of applicative-order evaluation

;; with normal-order evaluation it would return 0 immediatelly.
;; In Clojure we can simulate this with macro:
(defmacro test [x y]
  `(if (= ~x 0)
     0
     ~y))
(test 0 (p))
