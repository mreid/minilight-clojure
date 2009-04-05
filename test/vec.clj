(ns test.vec
	(:use vec clojure.contrib.test-is))

(deftest test-origin
	(is (origin? origin))
	(is (origin? [0 0 0])))

(deftest test-read-vec
	(is (= [1 2 3] (read-vec "(1 2 3)"))))

(deftest test-dot
	(is (= 0 (dot [1 1 1] [-1 1 0])))
	(is (= 2 (dot [1 2 3] [3 -2 1]))))

(deftest test-add
	(is (= [1 1 0] (add [1 1 0] origin)))
	(is (= [-2 3 4] (add origin [-2 3 4])))
	(is (= [1 2 3] (add [1 -1 2] [0 3 1]))))

(deftest test-scale
	(is (= [0 0 0] (scale 0 [1 2 3])))
	(is (= [-1 -2 -3] (scale -1 [1 2 3])))
	(is (= [2 4 6] (scale 2 [1 2 3]))))

(deftest test-cross
	(is (= [-3 6 -3] (cross [1 2 3] [4 5 6])))
	(is (= [0 0 1]   (cross [1 0 0] [0 1 0])))
	(is (= [0 0 0]   (cross [1 0 0] [1 0 0]))))

(deftest test-norm
	(is (= 0 (norm origin )))
	(is (= (Math/sqrt 3) (norm [1 1 1])))
	(is (= (Math/sqrt 2) (norm [1 0 -1]))))

(deftest test-approx=
	(is (approx= origin [0.000000001 0.000000001 -0.000000001])))

(deftest test-normalise
	(is (approx= [(/ 2 7) (/ 3 7) (/ 6 7)] (normalise [2 3 6]))))

(deftest test-clamp
	(is (= [1 0 0]         (clamp 0 1 [2 -1 -1])))
	(is (= [0.5 0.5 0.5]   (clamp 0 1 [0.5 0.5 0.5]))))

