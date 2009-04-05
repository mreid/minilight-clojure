(ns test 
	(:use test.vec test.triangle clojure.contrib.test-is))

(run-tests 'test.vec)	
(run-tests 'test.triangle)