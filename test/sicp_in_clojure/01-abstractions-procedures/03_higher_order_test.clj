(ns sicp-in-clojure.01-abstractions-procedures.03-higher-order-test
  (:require [clojure.test :refer [deftest is testing]]
            [sicp-in-clojure.01-abstractions-procedures.03-higher-order :as sut]
            [sicp-in-clojure.01-abstractions-procedures.exercise :as e]))

(deftest half-interval-method
  (testing "pi aproximation using sin"
    (is (= 3.14111328125
           (sut/half-interval-method e/sine 2.0 4.0))))
  (testing "root of the equation"
    (is (= 1.89306640625
           (sut/half-interval-method (fn [x] (- (* x x x)
                                                (* 2 x)
                                                3))
                                     1.0
                                     2.0)))))

(deftest fixed-point
  (testing "cosine fixed point approximation"
    (is (= 0.7390822985224024
           (sut/fixed-point #(Math/cos %) 1.0)))
    )
  (testing "sin(x) + cos(x) approximation"
    (is (= 1.2587315962971173
           (sut/fixed-point #(+  (Math/sin %) (Math/cos %)) 1.0))))
  (testing "square root approximation"
    (is (= 2.000000000000002
           (sut/sqrt 4)))))
  
