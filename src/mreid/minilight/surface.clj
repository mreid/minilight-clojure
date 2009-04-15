;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure --- Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ----------------------------------------------------------------------------
 
;; --- src/mreid/minilight/surface.clj ---
(ns mreid.minilight.surface
  (:use mreid.minilight.vec)
  (:use mreid.minilight.triangle))
 
(defstruct surface :triangle :position)

(defn emission
  "Returns the vector of emission values from the surfacepoint surface to the 
   vector to-pos. The vector out-dir is the main direction of the emission and
   the boolean solid-angle? determines whether the emission is scaled by 
   distance."
  [surface to-pos out-dir solid-angle?]
  (let [ ray      (sub to-pos (:position surface))
        triangle (:triangle surface)
        dist2    (dot ray ray)
        cosArea  (* (dot out-dir (normal triangle)) (area triangle))]

    (if (> cosArea 0)  ; Something to emit?
      (* (:emit triangle)
         (if solid-angle? (/ cosArea (max dist2 epsilon)) 1)) ;Solid => no scale 
      origin)))

(defn reflection
  "Returns the reflection vector resulting from the in-radiance vector passing 
   from in-dir to out-dir via the given surface."
  [surface in-dir in-radiance out-dir]
  (let [ triangle     (:triangle surface)
         surf-norm    (normal triangle)
         reflect      (:reflect triangle)
         in-proj      (dot in-dir surf-norm)
         out-proj     (dot out-dir surf-norm)]

   (if (>= (* in-proj out-proj) 0)  ; Projections on same side of surface
     (scale 
       (/ (Math/abs in-proj) Math/PI)
       (map * in-radiance reflect)) ; Point-wise product of rad. & reflect.
     origin)))

(defn rand-hemisphere
  "Returns a cosine-weight random 3d vector from a unit hemisphere"
  []
  (let [ a     (* 2 Math/PI (rand))
         b     (rand)
         sqrtb (Math/sqrt b)]
   [ (* (Math/cos a) sqrtb), (* (Math/sin a) sqrtb), (Math/sqrt (- 1 b)) ]))

(defn- sign [x] (if (>= x 0) 1 -1))
(defn- align [v1 v2] (scale (sign (dot v1 v2)) v1))

(defn next-direction
  "Computes the next direction and color vectors of a ray with in-direction 
   vector in-dir bouncing off the given surface"
  [surface in-dir]
  (let [ triangle     (:triangle surface)
         reflect      (:reflect triangle)
         reflect-mean (/ (reduce + reflect) (count reflect))]
    (if (< (rand) reflect-mean)
      (let [ coeffs      (rand-hemisphere) 
             normal-vec  (align (normal triangle) in-dir)
             tangent-vec (tangent triangle)
             cross-vec   (cross normal-vec tangent-vec)]
        [ (map scale coeffs [tangent-vec cross-vec normal-vec]) ; Direction
          (scale (/ 1 reflect-mean) reflect) ]                  ; Colour
      [origin, origin] ))))
