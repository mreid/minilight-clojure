;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure --- Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ---------------------------------------------------------------------------- 

;; Defines functions for working with geometrical vectors.
(ns vec)

(defn read-vec
	"Construct a new vector from a string of the form (<float> <float> <float>)"
	[instr]
	(map read-string
		(drop 1 (re-matches #"\((.*)\s(.*)\s(.*)\)" instr))))

(def origin [0 0 0])
(defn origin? [vector] (every? zero? vector))

; Binary operators
(defn dot [v1 v2] (reduce + (map * v1 v2)))
(defn add [v1 v2] (map + v1 v2))
(defn sub [v1 v2] (map - v1 v2))
(defn cross
	"Computes the cross product of v1 and v2. Assumes they are of length 3."
	[v1 v2] 
	[ 
		(- (* (v1 1) (v2 2)) (* (v1 2) (v2 1)))
		(- (* (v1 2) (v2 0)) (* (v1 0) (v2 2)))
		(- (* (v1 0) (v2 1)) (* (v1 1) (v2 0)))
	])

; Scalar operators
(defn scale [m v] (map #(* m %) v))

; Unary operators
(defn norm [v] (Math/sqrt (dot v v)))
(defn normalise [v] (scale (/ 1 (norm v)) v))

(defn det
	"Compute the determinant of vectors v1 v2 and v3 using
	det = v1.(v2 x v3)."
	
	[v1 v2 v3]
	(dot v1 (cross v2 v3)))

(def epsilon 0.000001)
(defn approx-0 [x] (< (Math/abs x) epsilon))
(defn approx= [v1 v2] (every? approx-0 (sub v1 v2)))

(defn invdet
	"Compute the inverse determinant of three vectors as 
	Returns nil if the det. is too close to 0"

	[v1 v2 v3]
	(let [d (det v1 v2 v3)]
		(if (approx-0 d)
			nil
			(/ 1.0 d))))

(defn clamper 
	[xmin xmax] 
	(fn [x] (max xmin (min xmax x))))

(defn clamp
	[xmin xmax v]
	(map (clamper xmin xmax) v))
