;;
;; Macros and functions in this file are likely
;; to be moved to system.lisp at some point.
;;

(defmacro w/object (object &rest forms)
    `(let ((<w/object>object ,object)
           (self (lambda () <w/object>object)))
        (dolist (<w/object>form ',forms)
            (eval (cons (first <w/object>form) (cons <w/object>object (rest <w/object>form)))))
        <w/object>object))

(set <- gencall)
