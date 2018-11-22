(ns sicp-in-clojure.01-abstractions-procedures.02-procedures-and-processes)

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



