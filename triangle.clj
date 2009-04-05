;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure --- Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ----------------------------------------------------------------------------

; Functions to build and interrogate triangles.
(ns triangle
	(:use vec))

(defstruct 
	#^{:doc "A triangle is three vertices with some other properties"}
	triangle
	:vertices		; Collection of 3 vectors
	:edge0			; Vector
	:edge3			; Vector
	:reflectivity	; Vector with all values in [0,1)
	:emitivity		; Vector with positive values
	:tangent		; Unit vector
	:normal			; Unit vector
	:area			; Float
)

(defn read-triangle [string] (map read-vec (re-seq #"\(.*?\)" string)))

(defn create
	"Creates a new triangle from a string of 5 3-tuples.
	The first three are vertex vectors, the fourth the reflectivity vector
	and the last the emitivity vector."
	
	[string]
	(let [  [v0 v1 v2 reflect emit] (read-triangle string) 
			edge0	(sub v1 v0)
			edge1	(sub v2 v1)
			edge3	(sub v2 v0)
			tangent	(normalise edge0)
			normal	(normalise (cross tangent edge1))
			pa2		(cross edge0 edge1)
			area	(/ (norm pa2) 2) ]
			
		(struct triangle
			[v0 v1 v2] edge0 edge3
			(clamp 0 (- 1 Float/MIN_VALUE) reflect) 
			(clamp 0 Float/MAX_VALUE emit)
			tangent normal area)))

; FIXME: This could be more efficient by only looping through vertices once.
(def TOLERANCE (/ 1.0 1024.0))
(defn tweak 
	[plus-or-minus]
	(fn [x]
		(plus-or-minus x (* (+ (Math/abs x) 1.0) TOLERANCE))))
(defn bounding-box
	"Computes the bounding box for a triangle, returning the result as
	a list of two vectors [lower-corner upper-corner]."

	[triangle]
	(let [vs (:vertices triangle)]
		[
			(map (tweak -) (apply map min vs))
			(map (tweak +) (apply map max vs))
		]))

(defn intersect
	"Find the intersection of the ray starting at ray0 in direction rayd
	with the triangle tri. 
	The returned value is a positive number t such that 
	ray0 + t.rayd is contained within tri, or nil if there is no such
	t."
	
	[tri ray0 rayd]
	(let [	e0 (:edge0 tri)
			e3 (:edge3 tri)
			invdet (invdet e0 rayd e3)
		 ]
		(if (number? invdet)
			(let [	v0 (first (:vertices tri))
					tv (sub ray0 v0)
					u  (* (dot v0 tv) invdet)
			]
			(if (and (>= u 0) (<= u 1))
				(let [	q (cross tv e0)
						v (* (dot rayd q) invdet)
				]
				(if (and (>= v 0) (<= (+ u v) 1))
					(let [t (* (dot e3 q) invdet)]
						(if (>= t 0) t))
				))
			))
		)
	))

(def rnd (java.util.Random.))
(defn sample-point
	"Sample a random point as a vector from inside the given triangle."
	
	[tri]
	(let [ 	sqr1 (Math/sqrt (.nextFloat rnd))
			r2	 (.nextFloat rnd)
			a	 (- 1 sqr1)
			b	 (* (- 1 r2) sqr1)
			v0	 (first (:vertices tri))
			e0	 (:edge0 tri)
			e3	 (:edge3 tri)
	]
	(add (add (scale a e0) (scale b e3)) v0)
	))
