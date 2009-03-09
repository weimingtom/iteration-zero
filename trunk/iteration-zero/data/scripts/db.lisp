
(defun make-material (filename &optional(color nil) (light nil))
    (w/object (make-instance c/material)
        (load-texture (join "data/materials/" filename))
        (set-color (first color) (second color) (third color) (fourth color))
        (set-light (first light) (second light) (third light) (fourth light) (fifth light))))

(defun load-material-def (path)
    "Load materials defs.lisp file from a directory under the data/materials path."
    (set filename (join *materials-path* path))
    (while (eq (right 1 filename) "/")
        (set filename (substring 0 (- (length filename) 1))))
    (set filename (join filename "/defs.lisp"))
    ;; Should have a valid filename now.
    ;; (println filename)

    ;; Prepare syntax for defs file.
    (set file
        (fn (fname) (join path "/" fname)))
    (set color 
        (fn (r g b a)
            (map tofloat (list r g b a))))
    (set light 
        (fn (r g b a f)
            (map tofloat (list r g b a f))))

    (set material
        (fn (name fname c l)
            (println fname )
            (list (tostring name)  (make-material fname c l))))

    ;; Parse the file
    (set materials
        (map eval (parse-file filename)))

    ;; Add list (name material)
    (dolist (mat materials)
        (apply (bind1st add-material *db*) mat)))







;; (defmacro load-materials (&rest table)
;;     `(map
;;         (lambda (name material) (add-material *db* name material))
;;         ,table))
;; 
;; (load-materials
;;     ("floor/mtlgrid" 
;; 
;; 
;;  = {
;;     color = (1 .9 .9 1)
;;     texture = "data/materials/nexmtl/mtl_flooring.jpg"
;; }
;; 
;; floor/mtlrost = {
;;     color = (1 .9 .9 1)
;;     texture = "data/materials/nexmtl/mtl_rstb.jpg"
;; }
;; 
;; wall/mtlstripe = {
;;     color = (1 .9 .9 1)
;;     texture = "data/materials/nexmtl/mtl_midwrn.jpg"
;; }

