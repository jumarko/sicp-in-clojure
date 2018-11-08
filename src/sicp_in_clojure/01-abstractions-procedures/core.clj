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


;;; 1.2.1 Factorials and recursion

;; one way to compute factorial is a simple recursion:


(defn fact [n]
  (if (<= n 1)
    1
    (* n (fact (dec n)))))
(fact 0)
;; => 1
(fact 5)
;; => 120

;; we can also take a different perspective
;; using an intereative approach: 1 * 2 * 3 * 4 * 5
;; Note: that in the book they also use `counter` and `max-count` arguments
;; but that's not strictly necessary for our computation
(defn facti [n]
  (letfn [(fact-iter [acc n]
            (if (<= n 0)
              acc
              (fact-iter (* n acc)
                         (dec n))))]
    (fact-iter 1 n)))
(facti 0)
;; => 1
(facti 5)
;; => 120




;;; 1.2.2 Tree Recursion (Fibonacci numbers et al.)

;; fibonacci numbers can be computed via straightforward recursive procedure:
(defn fibr [n]
  (cond
    (zero? n) 0
    (= 1 n) 1
    (< 1 n) (+ (fibr (- n 1))
               (fibr (- n 2)))))

(fibr 0)
(fibr 1)
(fibr 7)
;; Notice it takes a while to compute fibonacci even for relatively small n
#_(time (fibr 35))
;; => "Elapsed time: 2837.268809 msecs"

;; Compare this to iterative approach
(defn fibi [n]
  (letfn [(fib-iter [a b counter]
            (if (zero? counter)
              b
              ;; for larger n-s you'd have to use `+'` here to avoid integer overflow
              (fib-iter (+ a b) a (dec counter))))]
    (fib-iter 1 0 n)))

(fibi 0)
(fibi 1)
(fibi 7)
;; and it's much faster too:
(time (fibi 35))
;; => "Elapsed time: 0.0512 msecs"
