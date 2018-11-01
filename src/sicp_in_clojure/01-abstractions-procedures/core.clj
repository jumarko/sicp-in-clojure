(ns sicp-in-clojure.01-abstractions-procedures.core)

;;; 1.1.4 Compound Procedures

(defn square [x] (* x x))

(defn sum-of-squares [x y]
  (+ (square x) (square y)))

(sum-of-squares 3 4)

(defn f [a]
  (sum-of-squares (+ a 1) (* a 2)))

(f 5)
