;;
;; Macros and functions in this file are likely
;; to be moved to system.lisp at some point.
;;

(defvar *gensym-counter* 0)

(defun gensym (&optional(prefix "gensym"))
    (symbol (join prefix ":" (tostring (incput *gensym-counter*)))))

(unless (issym (gensym)) (error))
(when (eq (gensym) (gensym)) (error))

;;;; Correct implementation - doesn't work :-(

(defmacro w/object (object &rest forms)
    (let ((obj (gensym)) (form (gensym)))
        `(let ((,obj ,object))
            (dolist (,form ',forms)
                (eval (cons (first ,form) (cons ,obj (rest ,form)))))
            ,obj)))

;; (defmacro w/object (object &rest forms)
;;     `(let ((obj ,object))
;;         (dolist (form ',forms)
;;             (eval (cons (first form) (cons obj (rest form)))))
;;         obj))

(set <- gencall)

