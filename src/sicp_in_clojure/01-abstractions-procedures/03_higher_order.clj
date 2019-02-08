(ns sicp-in-clojure.01-abstractions-procedures.03-higher-order
  "Higher order procedures.
  Video Lecture 2A: https://www.youtube.com/watch?v=erHp3r6PbJk&list=PLE18841CABEA24090&index=3"
  (:require
   [sicp-in-clojure.01-abstractions-procedures.01-elements :as c]
   [sicp-in-clojure.01-abstractions-procedures.exercise :as e]))

;;; Similar patterns to refactor...
(defn sum-ints
  "Sum ints from a to b (inclusively)."
  [a b]
  (if (> a b)
    0
    (+ a (sum-ints (inc a) b))))
(sum-ints 1 10)
;; => 55

(defn sum-sq
  "Sum ints from a to b (inclusively)."
  [a b]
  (if (> a b)
    0
    (+ (c/square a)
       (sum-sq (inc a) b))))
(sum-sq 2 5)
;; => 54

(defn pi-sum
  "Computers a sequence `pi/8 = 1/1*3 + 1/5*7 + 1/9*11 + ...`.
  This is an interesting sequence originally discovered by Leibnitz
  and usually written as `pi/4 = 1 - 1/3 + 1/5 - 1/7 + 1/9 ...`."
  [a b]
  (if (> a b)
    0
    (+
     (/ 1 (* a (+ a 2)))
     (pi-sum (+ a 4) b))))

;; coerce to double to get more familiar result
(double (* 8 (pi-sum 1 1000)))
;; => 3.139592655589783


(defn sum
  "General summing procedure.
  `term` computes the current value (e.g. square root)
  `a` is the start of the interval
  `next` next is successing function (e.g. increment)
  `b` is the end of the interval (inclusive)"
  [term a next b]
  (if (> a b)
    0
    (+ (term a)
       (sum term (next a) next b))))
;; now let's redefine our former procedures
(defn sum-ints [a b]
  (sum identity a inc b))
(sum-ints 1 10)
;; => 55

(defn sum-sq [a b]
  (sum c/square a inc b))
(sum-sq 2 5)
;; => 54

(defn pi-sum [a b]
  (sum #(/ 1 (* % (+ % 2)))
       1
       #(+ % 4)
       1000))
(double (* 8 (pi-sum 1 1000)))
;; => 3.139592655589783



;;; Let's define sqrt in terms of general fixed-point function
(defn fixed-point [improve-fn start]
  (letfn [(iter [old new]
            (if (e/better-good-enough? old new)
              new
              (iter new (improve-fn new))))]
    (iter start (improve-fn start))))

(defn average-damp [f]
  (fn [x] (c/avg (f x) x)))

(defn sqrt [x]
  (fixed-point (average-damp (fn [y] (/ x y)))
               1))

(double (sqrt 4))
(double (sqrt 9))
(double (sqrt 100))

;;; SQRT in terms of General Newton method
;;; See the book - p. 74

;; start with sqrt & empty skeleton for newton's procedure ("wishfull thinking")
(defn newton [improve-fn guess])
(defn sqrt [x]
  (newton
   (fn [y] (- x (c/square y)))
   1))

;; now let's define newton fn:
(declare deriv)
(defn newton [f guess]
  (let [df (deriv f)]
    (fixed-point
     (fn [x] (- x
                (/ (f x) (df x))))
     guess)))

;; finally, let's implement `deriv`:
(defn deriv [f]
  (let [dx 0.00001] ;; let dx be very small number...
    (fn [x] (/ (- (f (+ x dx))
                  (f x))
               dx))))
(sqrt 4)


;;; Definite integral - (p. 59-60)

;; naive approach (p.59)
(defn integral
  "Approximates the value of definite integral of `f` between the limits `a` and `b`.
  Note: this implementation doesn't optimize tail-recursion so the dx shouldn't be smaller
  1/1000 of (b - a) )."
  [f a b dx]
  (let [add-dx #(+ % dx)]
    (* (sum f
            (+ a (/ dx 2))
            add-dx
            b)
       dx)))

(integral e/cube 0 1 0.01)
;; => 0.24998750000000042
(integral e/cube 0 1 0.001)
;; => 0.249999875000001
;; (integral e/cube 0 1 0.0001) => throws StackOverflow


;; Ex. 1.29: simpson's integral
;; The hard thing about this exercise are alternating factors (4 and 2)
;; which don't play nicely with the `term` and `next` functions used in the helper `sum` procedure.
;; The realization you need to make is that you can actually derive the proper coefficient (1, 4, or 2)
;; without complicating `next` function (which will be just `inc`):
(defn- coefficient [k n]
  (cond
    (or (= k 0) (= k n))
    1

    (odd? k)
    4

    (even? k)
    2))
(defn simpson-integral
  "Computes a definite integral of `f` between the limits `a` and `b` using
  the Simpson's rule.
  `n` determines number of iterations - the grater, the more accurate the result is."
  [f a b n]
  (let [h (/ (- b a) n)
        f-k (fn [k] (*
                     (coefficient k n)
                     (f (+ a (* k h)))))]
    (* (/ h 3)
       (sum f-k 0 inc n))))

(double (simpson-integral e/cube 0 1 5))
;; => 0.2032
(double (simpson-integral e/cube 0 1 20))
;; => 0.25
(double (simpson-integral e/cube 0 1 100))
;; => 0.25
(double (simpson-integral e/cube 0 1 1000))
;; => 0.25


;;; 1.30 Iterative version of `sum`
;; let's start by creating skeletons:
(defn sum-iter [acc term a next b]
  acc)
(defn sum [term a next b]
  (sum-iter 0 term a next b))
;; then we need to improve `sum-iter`
(defn sum-iter [acc term a next b]
  (if (> a b)
    acc
    ;; it' simple just add to accumulator
    (recur (+ acc (term a))
           term
           (next a)
           next
           b)))
;; now we can compute large sums
;; this would fail on StackOverflow before...
(sum-ints 1 10000)
;; => 50005000
;; Alternatively we can follow the book recommandation and save some arguments
(defn sum2 [term a next b]
  (let [iter (fn iter [a result]
               (if (> a b)
                 result
                 (recur (next a)
                        (+ result (term a)))))]
    (iter a 0)))
(sum2 identity 1 inc 10000)
;; => 50005000


;;; 1.31 product function
;;; Show how to define factorial via `product`
;;; Also computers pi approximation via John Wallis' formula pi/4 = 2 * 4 * 4 * 6 * 6 * 8 ... / (3 * 3 * 5 * 5 * 7 * 7)
(defn product-recursive [term a next b]
  (if (> a b)
    0
    (* (term a)
       (product-recursive term (next a) next b))))
(defn product-iterative [term a next b]
  (let [iter (fn iter [a result]
               (if (> a b)
                 result
                 (recur (next a)
                        (* result (term a)))))]
    (iter a 1)))
(def product product-iterative)
(defn ints-product [a b]
  (product identity a inc b))
(def factorial (partial ints-product 1))
(factorial 5)
;; => 120

(defn pi-wallis
  "Computes approximation of pi using John Wallis' formula:
  `pi/4 = (2 * 4 * 4 * 6 * 6 * 8 * ...) / (3 * 3 * 5 * 5 * 7 * 7 * ...)`"
  [n]
  ;; we need to realize that it's just about simple transformation in the `term` function
  ;; taking into account which element we're dealing with
  (let [term (fn term [a]
               (if (odd? a)
                 (/ (+ a 1) (+ a 2))
                 (/ (+ a 2) (+ a 1))))]
    (* 4
       (product term 1 inc n))))
(double (pi-wallis 10))
;; => 3.275101041334808
(double (pi-wallis 100))
;; => 3.157030176455168


