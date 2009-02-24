(load-dataset *level* "data/test/micro_dataset.dl")
(load-dataset *level* "data/test/micro_dataset.dl")

(init *level* 50 50)
(finalize *level*)

(defun place-tile (x y name)
    (place-tile-by-name *level* x y name))

(defun place-wall-x (x y d name)
    (dotimes (dx d)
        (place-tile (+ x dx) y name)))

(defun place-wall-y (x y d name)
    (dotimes (dy d)
        (place-tile x (+ y dy) name)))

;; (defmacro call-method (name object &rest args)
;;     `(let ((m (get-attr ,object ,name)))
;;         (funcall m (cons ,object ,args))))

;; (place-object-by-name *level* 5 5 "blade")

(place-tile 10 10 "wall")


(place-wall-x 0 0 50 "wall")
(place-wall-x 0 49 50 "wall")
(place-wall-y 0 0 50 "wall")
(place-wall-y 49 0 50 "wall")

