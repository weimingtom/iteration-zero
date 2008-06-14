;;
;; Macros and functions in this file are likely
;; to be moved to system.lisp at some point.
;;


;;;
;;;  Core stuff - should land in system.lisp
;;;

(defvar *gensym-counter* 0)

(defun gensym (&optional(prefix "gensym"))
    (symbol (join "<" prefix ":" (tostring (incput *gensym-counter*)) ">")))

(unless (issym (gensym)) (error))
(when (eq (gensym) (gensym)) (error))

(set <- gencall)


;;;
;;; General purpose macros
;;;

(defmacro w/object (object &rest forms)
    (let ((obj (gensym "w/object")) (form (gensym "w/object")))
        `(let ((,obj ,object))
            (dolist (,form ',forms)
;;                 (print "W/OBJECT:" ,obj "-->" ,form *ln*)
                (eval (cons (first ,form) (cons ,obj (rest ,form)))))
            ,obj)))

(defmacro deftable (name &rest table)
    `(defvar ,name ',table))

;;;
;;; List processing macros
;;;


(defmacro pop! (l)
    (let ((start (gensym)))
        `(progn
            (set ,start (first ,l))
            (set ,l (rest ,l))
            ,start))) 


(defmacro push! (v l)
    `(progn
        (set ,l (cons ,v ,l))
;;         (print "PUSH! " ,v "  " ,l *ln*)
        ,l))


;;;
;;;  Useful file handling functions
;;;

(defun read-lines (filename)
    (set data nil)
    (set file (open filename))
    (while (set line (read-line file))
        (set data (cons line data)))
    (reverse data)) 

(defun read-file (filename)
    (apply join (map (fn (line) (join line " " *ln*)) (read-lines filename))))

(defun parse-file (filename)
    (set f (open filename))
    (set retval nil)
    (catch (parse-state)
        (while (set token (parse f))
            (push! token retval)))
    retval)
