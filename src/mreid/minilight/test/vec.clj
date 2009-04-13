;; --- src/name/reid/mark/minilight/test/vec.clj ---
;; Tests for vec.clj using the test-is library.
(ns mreid.minilight.test.vec
    (:use mreid.minilight.vec) 
    (:use clojure.contrib.test-is))

(def dyn100 (sub [1 2 3] [0 2 3]))
(def dyn010 (sub [1 2 3] [1 1 3]))

(deftest test-approx0
    (is (approx0 -0.000000001))
    (is (not (approx0 0.001))))

(deftest test-clamp
    (is (= [1 0 0]       (clamp 0 1 [2 -1 -1])))
    (is (= [0.5 0.5 0.5] (clamp 0 1 [0.5 0.5 0.5]))))

(deftest test-dot
    (is (= 0 (dot [1 1 1] [-1 1 0])))
    (is (= 2 (dot [1 2 3] [3 -2 1]))))

(deftest test-add
    (is (= [1 1 0]  (add [1 1 0] origin)))
    (is (= [-2 3 4] (add origin [-2 3 4])))
    (is (= [1 2 3]  (add [1 -1 2] [0 3 1]))))

(deftest test-dynamic-add
    (is (= [1 1 0]  (add dyn100 dyn010))))

(deftest test-sub
    (is (= [1 2 3]  (sub [1 2 3] origin)))
    (is (= [0 0 0]  (sub [1 2 3] [1 2 3])))
    (is (= [-2 0 2] (sub [1 2 3] [3 2 1]))))

(deftest test-dynamic-sub
    (is (= [1 -1 0] (sub dyn100 dyn010))))

(deftest test-scale
    (is (= [0 0 0]    (scale 0 [1 2 3])))
    (is (= [-1 -2 -3] (scale -1 [1 2 3])))
    (is (= [2 4 6]    (scale 2 [1 2 3]))))

(deftest test-dynamic-scale
    (is (= [2 0 0]    (scale 2 dyn100))))

(deftest test-cross
    (is (= [-3 6 -3] (cross [1 2 3] [4 5 6])))
    (is (= [0 0 1]   (cross [1 0 0] [0 1 0])))
    (is (= [0 0 0]   (cross [1 0 0] [1 0 0]))))

(deftest test-dynamic-cross
    (is (= [0 0 1]   (cross dyn100 dyn010))))

(deftest test-norm
    (is (= 0 (norm origin )))
    (is (= (Math/sqrt 3) (norm [1 1 1])))
    (is (= (Math/sqrt 2) (norm [1 0 -1]))))

(deftest test-normalise
    (is (= [(/ 2.0 7.0) (/ 3.0 7.0) (/ 6.0 7.0)] (normalise [2 3 6]))))

(deftest test-det
    (is (= 1 (det [1 0 0] [0 1 0] [0 0 1])))
    (is (= 3 (det [1 1 0] [1 2 3] [4 5 6])))
    (is (= 0 (det [1 1 1] [1 2 3] [4 5 6]))))
    
(deftest test-invdet
    (is (= 1 (invdet [1 0 0] [0 1 0] [0 0 1])))
    (is (= (/ 1 3) (invdet [1 1 0] [1 2 3] [4 5 6])))
    (is (nil? (invdet [1 1 1] [1 2 3] [4 5 6]))))
