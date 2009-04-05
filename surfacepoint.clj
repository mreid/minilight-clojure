;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure --- Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ---------------------------------------------------------------------------- 

(ns surfacepoint)

(defstruct
	#^{:doc "A surface point is defined by a triangle and a position"}
	surfacepoint
	:triangle 		; A triangle
	:position		; A vector
)

(defn get-emission
	""
	[surf_pt to_position out_direction solid_angle?]
	(let [ 	ray 		(vec/sub to_position (:position surf_pt))
			distance2 	(vec/dot ray ray)
			normal		(:normal surf_pt)
			area		(:area surf_pt)
			cos_area	(* (vec/dot out_direction normal) area)
			solid_angle	(if (solid_angle?)
							(/ cos_area))]
		))