;; --- src/name/reid/mark/minilight/test/triangle.clj ---
;; Tests for triangle.clj using the test-is library.
(ns mreid.minilight.test.triangle
  (:use mreid.minilight.triangle)
  (:use clojure.contrib.test-is))

(def xytriangle
  (struct triangle 
     [ [0 0 0] [1 0 0] [0 1 0] ] ; Triangle in xy-plane
     [0.5 0.5 0.5]               ; Reflectivity
     [1 1 1]                     ; Emitivity 
  ))

(def y2ztriangle
  (struct triangle 
     [ [0 0 0] [0 2 0] [0 0 1] ] ; Triangle in xy-plane
     [0.5 0.5 0.5]               ; Reflectivity
     [1 1 1]                     ; Emitivity 
  ))

(deftest test-vertex
  (is (= [0 0 0] (vertex xytriangle 0)))
  (is (= [1 0 0] (vertex xytriangle 1)))
  (is (= [0 1 0] (vertex xytriangle 2)))

  (is (= [0 0 0] (vertex y2ztriangle 0)))
  (is (= [0 2 0] (vertex y2ztriangle 1)))
  (is (= [0 0 1] (vertex y2ztriangle 2))))
    
(deftest test-edge
  (is (= [1 0 0] (edge xytriangle 0 1)))
  (is (= [0 1 0] (edge xytriangle 0 2)))
  (is (= [-1 1 0] (edge xytriangle 1 2)))
  (is (= [1 -1 0] (edge xytriangle 2 1)))
  
  (is (= [0 2 0] (edge y2ztriangle 0 1)))
  (is (= [0 0 1] (edge y2ztriangle 0 2)))
  (is (= [0 -2 1] (edge y2ztriangle 1 2)))
  (is (= [0 2 -1] (edge y2ztriangle 2 1))))

(deftest test-tangent
  (is (= [1 0 0] (tangent xytriangle)))
  (is (= [0 1 0] (tangent y2ztriangle))))
    
(deftest test-normal
  (is (= [0 0 1] (normal xytriangle)))
  (is (= [2 0 0] (normal y2ztriangle))))

(deftest test-unit-normal
  (is (= [0 0 1] (unit-normal xytriangle)))
  (is (= [1 0 0] (unit-normal y2ztriangle))))
    
(deftest test-area
  (is (= 0.5 (area xytriangle)))
  (is (= 1   (area y2ztriangle))))

(deftest test-intersect)
  (is (= 1 (intersect xytriangle [0 0 1] [0 0 -1])))