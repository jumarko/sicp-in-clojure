(ns sicp-in-clojure.01-abstractions-procedures.02-procedures-and-processes
  (:require
   [sicp-in-clojure.core :refer [get-stack get-stack-depth]]))

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
              (fib-iter (+' a b) a (dec counter))))]
    (fib-iter 1 0 n)))

(fibi 0)
(fibi 1)
(fibi 7)
;; and it's much faster too:
(time (fibi 35))
;; => "Elapsed time: 0.0512 msecs"
(fibi 100)


;;; 1.2.2 Example: Counting Change (p. 40)
;;; Write a procedure to compute the number of ways to change ani given amount of money
;;; given that we have half-dollars, quarters, dimes, nickles, pennies
(defn- first-denomination [kinds-of-coins]
  (condp = kinds-of-coins
    1 1
    2 5
    3 10
    4 25
    5 50))

(defn change
  ([amount] (change amount 5))
  ([amount kinds-of-coins]
   (cond
     (zero? amount)
     1
     (or (neg? amount) (zero? kinds-of-coins))
     0

     :else
     (+ (change amount (dec kinds-of-coins))
        (change (- amount (first-denomination

                           kinds-of-coins)))))))

;;; WTF??
(change 10)



;;; 1.2.4 Exponentiation
;;; Fast exponentiation algorithm using succcessive squaring
;;; See also exercise.clj for iterative version

(defn exp [base n]
  (if (zero? n)
    1
    ;; notice `*'` for arbitrary precision
    (*' base (exp base (dec n)))))
(time (exp 2 10) )
;; be careful to avoid stackoverflow -> use loop-recur in `exp`
;; if you want to support exponents >> 1000
(time (exp 2 1000) )
;; => "Elapsed time: 0.62659 msecs"

(defn square' [x] (*' x x))

(defn fast-exp
  [base n]
  (cond
    (zero? n)
    1

    (even? n)
    (square' (fast-exp base (/ n 2)))

    :else
    (*' base (fast-exp base (dec n)))))

(fast-exp 2 10)
(time (fast-exp 2 1000))
;;=> "Elapsed time: 0.094528 msecs"

;; and we can support much larger exponents without loop-recur too
(time (fast-exp 2 100000))
;;=> "Elapsed time: 1.444502 msecs"



;;; 1.2.5 Greatest Common Divisor (GCD) - Euclid's algorithm
;;; Naive approach is to factor numbers and find common factors
;;; However, Euclid's algorithm is much more efficient

(defn gcd [a b]
  #_(println "stack depth: " (get-stack-depth "gcd"))
  (if (zero? b)
    a
    (gcd b (rem a b))))

(gcd 206 40)
(gcd 2793 1113) ;; 5 recursive calls
;; => 2



;;; 1.2.6 Probabilitistic algos, prime numbers, Fermat's test
;;;

;; to check whether number is a prime we need to find the smallest divisor.
;; if that's n than we know it's a prime

(defn find-divisor
  "Finds the first divisor of number n starting from start-divisor."
  [n start-divisor]
  (cond
    ;; "trick" with square to avoid unnecessary computation
    (< n (square' start-divisor))
    n

    (zero? (rem n start-divisor))
    start-divisor

    :else
    (find-divisor n (inc start-divisor))))

(defn smallest-divisor [n]
  (find-divisor n 2))

(defn prime? [n]
  (= n (smallest-divisor n)))

(prime? 2)
;; => true
(prime? 4)
;; => false
(prime? 19)
;; => true
(prime? 41)
;; => true
(prime? 81)
;; => false
