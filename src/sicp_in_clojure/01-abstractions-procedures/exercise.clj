(ns sicp-in-clojure.01-abstractions-procedures.exercise
  "All 46 exercises from the Chapter 1."
  (:require [sicp-in-clojure.01-abstractions-procedures.01-elements :as c]))

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


;;; 1.9 (p.36)
;;; Use substitution model to illustrate the process generated by each procedure
;;; in evaluating `(+ 4 5)
;;; Are these processes iterative or recursive?
;;; 
;;; Note: you can also look at Joy of Clojure - Chapter 7.3: Thinking Recursively https://github.com/danielmiladinov/joy-of-clojure/blob/master/src/joy-of-clojure/chapter7/thinking_recursively.clj
;;; Check https://wizardbook.wordpress.com/2010/11/24/exercise-1-9/
(defn plus [a b]
  (if (zero? a)
    b
    (inc (plus (dec a) b))))

;;=> recursive
(plus 4 5)
(inc (plus 3 5))
(inc (inc (plus 2 5)))
(inc (inc (plus 2 5)))
(inc (inc (inc (plus 1 5))))

(inc (inc (inc (inc (plus 0 5)))))

(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9


(defn plus2 [a b]
  (if (zero? a)
    b
    (plus2 (dec a) (inc b))))

;;=> iterative
(plus2 4 5)
(plus2 3 6)
(plus2 2 7)
(plus2 1 8)
(plus2 0 9)
9


;;; 1.10 (p.36) - Ackermann's function
;;; Check this fun web presentation from Garry Fredericks: https://gfredericks.com/sandbox/arith/ackermann
;;; Check https://wizardbook.wordpress.com/2010/11/24/exercise-1-10/
(defn A [x y]
  (cond
    (zero? y) 0
    (zero? x) (* 2 y)
    (= 1 y) 2
    :else (A (dec x)
             (A x (dec y)))))
(A 1 10)
;; => 1024

(A 2 4)
;; => 65536
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)))
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)))
(A 1 (A 1 (* 2 2)))
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
(A 0 (A 0 (A 0 (A 0 (A 1 12)))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 11))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 10)))))))
;; ...

(A 3 3)
;; => 65536

;;; Give us concise defintions for following functions
(def f (partial A 0))
(f 10)
;; => 20
;; => f: 2*n

(def g (partial A 1))
(g 10)
;; => 1024
;; => g: 2^n

(def h (partial A 2))
(h 1)
;; => 2
(h 2)
;; => 4
(h 3)
;; => 16
(h 4)
;; => 65536
;; (h 5)
;; => 2^65536 (stackoverflow)
;; => h: 2^(2^(2^(2^(... )))) // there are n twos in this definition


;;; Ex. 1.11 (p. 42)
;; recursive solution
(defn f3 [n]
  (if (< n 3)
    n
    (+ (f3 (- n 1))
       (* 2 (f3 (- n 2)))
       (* 3 (f3 (- n 3))))))
(f3 2)
(f3 3)
(f3 5)

;; iterative solution
(defn f3iter
  [a b c counter]
  (if (zero? counter)
    c
    (+ (f3iter (+ a (* 2 b) (* 3 c))
           a
           b
           (dec counter)))))
(defn f3i
  [n]
  {:pre [(nat-int? n)]}
  (f3iter 2 1 0 n))
(comment 
  (f3i -1)
  (f3i 0)
  (f3i 1)
  (f3i 2)
  (f3i 3)
  (f3i 5)
  (f3i 6))


;;; Ex. 1.12 (p. 42) Pascal's Triangle
(defn pascal-row
  "Computes nth row of pascals triangle starting from the row 0."
  [previous-row]
  (mapv +'
        (cons 0 previous-row)
        (concat previous-row [0])))
(defn pascal
  "Returns first n rows of pascal's triangle."
  [n]
  (let [initial-row [1]]
    (take n (iterate pascal-row initial-row))))
(comment
  (pascal 5)

  )


;;; Ex. 1.13 (p.42) Proof -> skipped.
;;; This could be based on polynoms exponents (see Pascal triangel)


;;; Ex 1.14 (p. 44)
;;; See https://wizardbook.wordpress.com/2010/11/24/exercise-1-14/


;;; Ex 1.15 (p. 44)
;;; sine approximation

(defn cube [x] (* x x x))

(defn p [x]
  (- (* 3 x)
     (* 4 (cube x))))

(defn sine [x]
  (if (< (c/abs x) 0.1)
    ;; angle is small enough therefore (sine x) is very close to the x itself
    x
    (p (sine (/ x 3)))))

(sine 0.523)

;; evaluated five times:
(sine 12.15)

;; order of growth:
;; - space: log3(n)
;; - time: log3(n)
;; Rationale: 0.1 is constant and in three steps we're roughly at 3: 0.1 * 3 * 3 *3 => 2.7


;;; Ex. 1.16 (p. 46))
;;; Fast exponentiation via iterative algorithm

(defn square' [x] (*' x x))

(defn- fast-exp-iter [a b n]
  (cond
    (zero? n)
    a

    (even? n)
    (fast-exp-iter a (square' b) (/ n 2))

    :else
    (fast-exp-iter (* a b) b (dec n))))

(defn fast-exp
  [base exponent]
  (fast-exp-iter 1 base exponent))

(fast-exp 3 3)
(fast-exp 3 6)
(fast-exp 3 7)
(fast-exp 4 6)
(fast-exp 5 5)
(fast-exp 2 5)
(fast-exp 2 10)

