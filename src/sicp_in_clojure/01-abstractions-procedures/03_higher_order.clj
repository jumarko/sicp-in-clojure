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
