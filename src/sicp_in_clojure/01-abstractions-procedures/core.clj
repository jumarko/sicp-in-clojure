(ns sicp-in-clojure.01-abstractions-procedures.core)

;;; 1.1.4 Compound Procedures

(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)

(defn abs [x]
  (if (< x 0) (- x) x))
(abs 10)
(abs 0)
(abs -10)

;;; 1.1.7 Example: Square Roots by Newton's Method
;; the first quite bad implementation of `good-enough?`
;; it's bad because it doesn't really for for either small or large numbers
(defn good-enough? [guess x]
  (< (abs (- (square guess)
             x))
     0.001))

(defn avg [x y]
  (/ (+ x y) 2))

(defn improve [guess x]
  (avg guess (/ x guess)))

(defn sqrt-iter
  [guess x]
  (if (good-enough? guess x)
    guess
    (sqrt-iter (improve guess x)
               x)))
(defn sqrt [x]
  (-> (sqrt-iter 1 x)
      double))

(sqrt 9)

(sqrt (+ 100 37))

(sqrt (+ (sqrt 2) (sqrt 3)))

(square (sqrt 1000))
