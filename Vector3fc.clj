;; ---------------------------------------------------------------------------- 
;; MiniLight in Clojure
;; 
;; Mark Reid <http://mark.reid.name/>
;;
;; Based on the Ruby code and architecture by Harrison Ainsworth / HXA7241.
;; <http://www.hxa7241.org/>
;; ---------------------------------------------------------------------------- 
(ns vec)

(defn origin? [vector] (every? zero? vector))

(defn x [vector] (nth vector 0))
(defn y [vector] (nth vector 1))
(defn z [vector] (nth vector 2))

(defn dot [v1 v2] (reduce + (pmap * v1 v2)))

(defn add [v1 v2] (pmap + v1 v2))
(defn sub [v1 v2] (pmap - v1 v2))

;
(defn 
	#^{:test (fn [] 
			(assert (= [-3 6 -3] (cross [1 2 3] [4 5 6])))
			(assert (= [0 0 1] (cross [1 0 0] [0 1 0])))
		)}
	cross
	"Computes the cross product of v1 and v2. Assumes they are of length 3."
	[v1 v2] 
	[ 
		(- (* (y v1) (z v2)) (* (z v1) (y v2)))
		(- (* (z v1) (x v2)) (* (x v1) (z v2)))
		(- (* (x v1) (y v2)) (* (y v1) (x v2)))
	])

; Run some tests
(test #'cross)