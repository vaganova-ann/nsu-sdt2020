(ns test-lab2
  (:use lab2.core)
  (:require [clojure.test :as test])
  )

(test/deftest test-lab2
              
 (test/testing "test 2.1"
   (test/is (= (integrate (fn [x] 1) 100 2) 100.0))
   (test/is (= (integrate (fn [x] 1) 100 3) 100.0))
   (test/is (= (integrate (fn [x] x) 14 3) 98.0))
   (test/is (= (integrate (fn [x] x) 14 5) 98.0)))
              
   (test/testing "test 2.2"
     (test/is (= ((get_lazy_integrate (fn [x] 1) 2) 100) 100.0))
     (test/is (= ((get_lazy_integrate (fn [x] 1) 3) 100) 100.0))
     (test/is (= ((get_lazy_integrate (fn [x] x) 3) 14) 98.0))
     (test/is (= ((get_lazy_integrate (fn [x] x) 5) 14) 98.0))
                 )           
 )

(test/run-tests 'test-lab2)