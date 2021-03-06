;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure --- Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ----------------------------------------------------------------------------
 
;; --- src/mreid/minilight/triangle.clj ---
;; A structure and functions for defining and querying triangles.
(ns mreid.minilight.triangle
  (:use mreid.minilight.vec))
 
(defstruct triangle
  :vertices ; Collection of 3 vectors
  :reflect  ; Vector with all values in [0,1)
  :emit     ; Vector with positive values
)

(defn vertex
  "Returns (the zero-indexed) vertex i in the triangle t"
  [t i] (nth (:vertices t) i))

(defn edge
  "Returns the edge in the triangle t from vertex i to vertex j"
  [t i j] (sub (vertex t j) (vertex t i)))

(defn tangent
  "Returns a unit vector tangent to the given triangle t"
  [t] (normalise (edge t 0 1)))

(defn normal
  "Returns a vector normal to the given triangle t (edge01 x edge12)"
  [t] (cross (edge t 0 1) (edge t 1 2)))

(defn unit-normal
  "Returns a unit vector normal to the given triangle t"
  [t] (normalise (normal t)))

(defn area
  "Returns the are of the given triangle t"
  [t] (/ (norm (normal t)) 2))

(def TOLERANCE (/ 1.0 1024.0))
(defn tweak
  "Returns a function that adds or subtracts a small amount"
  [add-or-sub]
  (fn [x] (add-or-sub x (* (+ (Math/abs x) 1.0) TOLERANCE))))

(defn bounding-box
  "Computes the bounding box for a triangle t, returning the result as
  a list of two vectors [lower-corner upper-corner]." 
  [t]
  (let [vs (:vertices t)]
    [ (map (tweak -) (apply map min vs))
      (map (tweak +) (apply map max vs)) ]))
 
(defn intersect
  "Finds the intersection with the triangle t of the ray starting at r0 in 
  direction rd. The returned value is a positive number a such that r0 + a.rd 
  is contained within t, or nil if there is no such a."
  [t r0 rd]
  (let [ e01    (edge t 0 1)
         e02    (edge t 0 2)
         p      (cross rd e02)
         invdet (invdet e01 rd e02) ]
    (if (number? invdet)
      (let [ v0 (vertex t 0)
             tv (sub r0 v0)
             u  (* (dot tv p) invdet) ]
        (if (and (>= u 0) (<= u 1))
          (let [ q (cross tv e01)
                 v (* (dot rd q) invdet) ]
            (if (and (>= v 0) (<= (+ u v) 1))
              (let [a (* (dot e02 q) invdet)]
                (if (>= a 0) a)))))))))
 
(defn sample-point
  "Returns a random point as a vector from inside the given triangle t"
  [t]
  (let [ sqr1 (Math/sqrt (rand))
         r2   (rand)
         a    (- 1 sqr1)
         b    (* (- 1 r2) sqr1) ]
    (add (vertex t 0)
        (add (scale a (edge t 0 1)) 
             (scale b (edge t 0 2))))))