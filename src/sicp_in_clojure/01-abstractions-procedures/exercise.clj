(ns sicp-in-clojure.01-abstractions-procedures.exercise
  "All 46 exercises from the Chapter 1."
  (:require [sicp-in-clojure.01-abstractions-procedures.core :as c]))

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


;;; Ex. 1.6 (p.25)
;;; Not seeing why `if` has to be special form...


(defn new-if [pred then else]
  (cond
    pred then
    :else else))
(new-if (= 2 3) 0 5)
;; => 5
(new-if (= 1 1) 0 5)
;; => 0

;; now let's try to rewrite square root with it
(defn sqrt-iter [guess x]
  (new-if (c/good-enough? guess x)
          guess
          (sqrt-iter (c/improve guess x)
                     x)))
(defn sqrt [x]
  (-> (sqrt-iter 1 x)
      double))
;; what happens now ?!
;; (sqrt 4)
;; => infinite loop (no stack overflow because it's iterative - tail recursion)


;;; Ex. 1.7 (p.25)
;;; See solution: http://community.schemewiki.org/?sicp-ex-1.7
;;; Improve good-enough? fn to work well with small and large numbers.
;;; Existing implementation is bad for small numbers because it the test is based on the absolute difference
;;; between the reality and the guess which just doesn't work for small numbers:
(c/sqrt 0.000001)
;; => 0.031260655525445276
(* 0.031260655525445276 0.031260655525445276)
;; => 9.772285838805523E-4
;;; It's also inappropriate for large numbers because of limited preciion of arithmentic operations
;; (But it's also really slow so we cannot effectively test it)
;; (c/sqrt 1000000)


;; to improve:
;; Track how guess changes from one iteration to another and stop
;; once the change is a small fraction of the guess
(defn better-good-enough? [old-guess new-guess]
  (< (c/abs (/ (- old-guess new-guess)
               new-guess))
     0.001))

(defn root-iter
  ([improve-fn guess x] (root-iter improve-fn x guess x))
  ([improve-fn old-guess new-guess x]
   (if (better-good-enough? old-guess new-guess)
     new-guess
     (root-iter
      improve-fn
      new-guess
      (improve-fn new-guess x)
      x))))

(defn sqrt
  [x]
  (-> (root-iter c/improve 1 x)
      double))

(sqrt 4)
;; => 2.000000092922295
(sqrt 0.000001)
;; => 0.0010000001533016628


;;; 1.8 (p.26)
;;; Newton's formular for cube roots. Better guess is derived via following formula:
;;; (x/y^2 + 2y) / 3
;;; Check solution: https://wizardbook.wordpress.com/2010/11/24/exercise-1-8/
(defn cube-improve [guess x]
  (let [y guess]
    (/ (+ (/ x (* y y))
          (* 2 y))
       3)))

(defn cube-root [x]
  (-> (root-iter cube-improve 1 x)
      double))

(cube-root 8)
(cube-root 27)
(cube-root -8)
