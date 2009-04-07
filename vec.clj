;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure --- Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ---------------------------------------------------------------------------- 

;; --- vec.clj ---
;; A simple vector package that defines functions for working with geometrical 
;; vectors.
(ns vec)

; Constants
(def origin [0 0 0])    ; Zero vector in 3D
(def epsilon 0.000001)  ; Tolerance for equality

; Helper functions
(defn approx0 
    "Returns true iff the value x is within epsilon of 0"
    [x] (< (Math/abs x) epsilon))

(defn clamp
    "Constrains all elements in v to be between vmin and vmax"
    [vmin vmax v] (map (fn [x] (max vmin (min vmax x))) v))

; Binary operators
(defn dot 
    "Returns the value of dot product of the vectors v1 and v2"
    [v1 v2] (reduce + (map * v1 v2)))
    
(defn add 
    "Returns a vector that is the sum of the vectors v1 and v2"
    [v1 v2] (map + v1 v2))
    
(defn sub 
    "Returns a vector that when added to v1 gives v2"
    [v1 v2] (map - v1 v2))

(defn cross 
    "Returns the cross product vector for the 3D vectors v1 and v2."
    [v1 v2] 
    [ (- (* (v1 1) (v2 2)) (* (v1 2) (v2 1)))
      (- (* (v1 2) (v2 0)) (* (v1 0) (v2 2)))
      (- (* (v1 0) (v2 1)) (* (v1 1) (v2 0))) ])

; Scalar operators
(defn scale 
    "Returns the vector that is m times the vector v"
    [m v] (map #(* m %) v))

; Unary operators
(defn norm 
    "Returns the (Euclidean) length of the vector v"
    [v] (Math/sqrt (dot v v)))

(defn normalise 
    "Returns a vector of unit length in the same direction as v"
    [v] (scale (/ 1 (norm v)) v))

; Determinants
(defn det 
    "Returns the determinant of vectors v1 v2 and v3, i.e. v1.(v2 x v3)"    
    [v1 v2 v3] (dot v1 (cross v2 v3)))

(defn invdet
    "Returns the inverse det. of v1, v2 and v3 or nil if it is too close to 0"
    [v1 v2 v3]
    (let [d (det v1 v2 v3)]
        (if (approx0 d)
            nil
            (/ 1.0 d))))
