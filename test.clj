(ns test 
	(:require vec)
	(:use clojure.contrib.test-is))
	
(deftest test-origin
	(is (vec/origin? vec/origin))
	(is (vec/origin? [0 0 0])))

(deftest test-create
	(is (= [1 2 3] (vec/create "(1 2 3)"))))

(deftest test-cross-product
	(is (= [-3 6 -3] (vec/cross [1 2 3] [4 5 6])))
	(is (= [0 0 1]   (vec/cross [1 0 0] [0 1 0]))))

(deftest test-clamp
	(is (= [1 0 0]         (vec/clamp 0 1 [2 -1 -1])))
	(is (= [0.5 0.5 0.5]   (vec/clamp 0 1 [0.5 0.5 0.5]))))

(run-tests)