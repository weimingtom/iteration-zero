;;;
;;;  Basically just a list of all material packages.
;;;

(deftable *material-defs*
    "nexmtl"
;;     "rock"
;;     "rrock1"
)

(defun load-all-material-defs ()
    (map load-material-def *material-defs*))