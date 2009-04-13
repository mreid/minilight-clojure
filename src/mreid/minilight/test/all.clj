(ns mreid.minilight.test.all
	(:gen-class)
	(:use mreid.minilight.test.vec)
	(:use mreid.minilight.test.triangle)
	(:use clojure.contrib.test-is))

(defn -main []
	(run-tests 'mreid.minilight.test.vec)
	(run-tests 'mreid.minilight.test.triangle))
