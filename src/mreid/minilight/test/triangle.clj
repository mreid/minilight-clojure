;; --- src/mreid/minilight/test/triangle.clj ---
;; Tests for triangle.clj using the test-is library.
(ns mreid.minilight.test.triangle
  (:use mreid.minilight.vec)
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
     [ [0 0 0] [0 2 0] [0 0 1] ] ; Triangle in yz-plane
     [0.5 0.5 0.5]               ; Reflectivity
     [1 1 1]                     ; Emitivity 
  ))

(def zxtriangle
  (struct triangle 
     [ [-10 5 -10] [-9 5 -10] [-10 5 -9] ] ; Triangle parallel to zx-plane
     [0.5 0.5 0.5]                         ; Reflectivity
     [1 1 1]                               ; Emitivity 
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

(def tweak+0 (* 1 TOLERANCE))
(def tweak-0 (* -1 TOLERANCE))
(def tweak+1 (+ 1 (* 2 TOLERANCE)))
(def tweak-1 (- 1 (* 2 TOLERANCE)))
(def tweak+2 (+ 2 (* 3 TOLERANCE)))
(def tweak-2 (- 2 (* 3 TOLERANCE)))

(deftest test-tweak
  (is (= tweak+0 ((tweak +) 0)))
  (is (= tweak-0 ((tweak -) 0)))
  (is (= tweak+1 ((tweak +) 1)))
  (is (= tweak-1 ((tweak -) 1)))
  (is (= tweak+2 ((tweak +) 2)))
  (is (= tweak-2 ((tweak -) 2))))

(deftest test-bounding-box
  (is (= [[tweak-0 tweak-0 tweak-0] [tweak+0 tweak+2 tweak+1]] 
         (bounding-box y2ztriangle))))

(deftest test-intersect
  (is (= 1 (intersect xytriangle [0 0 1] [0 0 -1])))
  (is (= 2 (intersect xytriangle [0 0 2] [0 0 -1])))
  (is (= 1 (intersect xytriangle [0.9 0 1] [0 0 -1])))
  (is (= 1 (intersect xytriangle [0.1 0.1 -1] [0 0 1]))))

(deftest test-no-intersect
  (is (nil? (intersect xytriangle [0 0 1] [0 0 1]))) ; Dir. is opposite
  (is (nil? (intersect xytriangle [0 0 1.1] [1 0 0]))) ; Dir. is parallel
  (is (nil? (intersect xytriangle [0 0 2] [0 1 -1])))) ; Goes wide

(defn random-ray
  "Returns [r0 rd] where rd is the unit normal of the triangle t and r0 is a 
   random point on t translated by -rd" 
  [t]
  (let [ray-direction (unit-normal t)]
    [ (sub (sample-point t) ray-direction) 
      ray-direction ]))

(deftest test-sample-point
  (let [xy-random-ray (random-ray xytriangle)
        zx-random-ray (random-ray zxtriangle)]
    (is (= 1 (apply intersect xytriangle xy-random-ray)))
    (is (= 1 (apply intersect zxtriangle zx-random-ray)))))
