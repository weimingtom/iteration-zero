(load-dataset *level* "data/test/dataset.dl")

(init *level* 100 100)
(finalize *level*)

(place-object-by-name *level* 3 3 "rena")

(place-object-by-name *level* 3 0 "smasher")
(place-object-by-name *level* 3 2 "sligg")


(place-object-by-name *level* 0 0 "blade")
;;(place-object-by-name *level* 1 1 "spider_dalek")
(place-object-by-name *level* 2 2 "pilot")

(place-object-by-name *level* 5 2 "forktruck")
(place-object-by-name *level* 10 2 "pallette")
(place-object-by-name *level* 10 10 "pallette/filled")
(place-object-by-name *level* 15 2 "barrel1")
(place-object-by-name *level* 15 5 "barrel2")
(place-object-by-name *level* 20 2 "box")
(place-object-by-name *level* 20 5 "pipes")
(place-object-by-name *level* 20 7 "rounds")


;; (place-object-by-name *level* 20 20 "triax/wheels")
;; (place-object-by-name *level* 10 20 "triax/hover")
;; (place-object-by-name *level* 5 20 "triax/tracks")

(place-object-by-name *level* 20 20 "turret1")
(place-object-by-name *level* 10 20 "turret2")
;; (place-object-by-name *level* 5 20 "fish")
(place-object-by-name *level* 15 20 "phoenix")
;; (place-object-by-name *level* 2 10 "plant1")
(place-object-by-name *level* 2 20 "plant2")
;; (place-object-by-name *level* 5 20 "triax/tracks")
