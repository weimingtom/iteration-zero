;;;
;;; SET
(if (equal (set a 1 b 2 c 3) 3) "OK set1" "NOK set1")
(if (equal (list a b c) '(1 2 3)) "OK set2" "NOK set2") 
(if (equal (set a (1+ b) b (1+ a) c (+ a b)) 7) "OK set3" "NOK set3")
(if (equal (list a b c) '(3 4 7)) "OK set4" "NOK set4")

;;;
;;; PUT
(if (equal (set x (cons 'a 'b) y (list 1 2 3)) '(1 2 3)) "OK put1" "NOK put1")
(if (equal (put (first x) 'x (first (rest y)) (first x) (rest x) y) '(1 X 3)) "OK put2" "NOK put2")
(if (equal x '(X 1 X 3)) "OK put3" "NOK put3")
(if (equal y '(1 X 3)) "OK put4" "NOK put4")

;;;
;;; QUOTE
(if (equal (quote a) 'a) "OK quote1" "NOK quote1")
(if (equal (set a 1) 1) "OK quote2" "NOK quote2")
(if (equal (quote (setq a 3)) '(SETQ A 3)) "OK quote3" "NOK quote3")
(if (equal (first '(a b)) 'a) "OK quote4" "NOK quote4")

;;;
;;; LAMBDA
(if (equal ((lambda (x) (+ x 3)) 4) 7) "OK lambda1" "NOK lambda1")
(if (equal ((lambda () "foo")) "foo") "OK lambda2" "NOK lambda2")

;;;
;;; EVAL
(if (equal (setq form '(1+ a) a 999) 999) "OK eval1" "NOK eval1")
(if (equal (eval form) 1000) "OK eval2" "NOK eval2")
(if (equal (eval 'form) '(1+ A)) "OK eval3" "NOK eval3")
(if (equal (let ((a 41)) (eval form)) 42) "OK eval4" "NOK eval4")

;;;
;;; PARSE
(if (equal (set foo (parse "(+ 1 2)")) '(+ 1 2)) "OK parse1" "NOK parse1")
(if (equal (eval foo) 3) "OK parse2" "NOK parse2")
(if (equal (eval (parse "(+ 1 2)")) 3) "OK parse3" "NOK parse3")
(if (equal (eval (parse (open "testcases/validateparse.lisp"))) 9) "OK parse4" "NOK parse4")

;;;
;;; DEFUN
(if (defun ortest (a &optional (b 2) &rest c) (list a b c)) "OK defun1" "NOK defun1")
(if (equal (ortest 1) '(1 2 NIL)) "OK defun2" "NOK defun2")
(if (equal (ortest 1 (1+ 2) (1+ 3)) '(1 3 (4))) "OK defun3" "NOK defun3")
(if (equal (ortest 1 2 3 4) '(1 2 (3 4))) "OK defun4" "NOK defun4")

;;;
;;; DEFMACRO
(if (defmacro inc (x) (list 'setq x (list '1+ x))) "OK defmacro1" "NOK defmacro1")
(if (equal (setq a 41) 41) "OK defmacro2" "NOK defmacro2") 
(if (equal (inc a) 42) "OK defmacro3" "NOK defmacro3")
(if (equal a 42) "OK defmacro4" "NOK defmacro4")


(defclass standard-object)
(defclass *instance-type*)
;;;
;;; OBJECTS
(if (isobject *type*) "OK object1" "NOK object1")
(if (not (isobject nil)) "OK object2" "NOK object2")
(if (isobject (type-of *type*)) "OK object3" "NOK object3")
(if (isobject standard-object) "OK object4" "NOK object4")


(defun get-method (object name)
    (if (has-attr object name)
        (get-attr object name)
        (if (isobject (get-attr object 'superclass))
            (get-method (get-attr object 'superclass) name))))

(defun call-method (object name &rest args)
    (funcall (get-method object name) (cons object args)))


(defclass A standard-object)

(defmacro defgeneric (function-name)
    `(defun ,function-name (object &rest args)
        (funcall (get-method object ',function-name) (cons object args))))

;; (set-attr standard-object 'repr (lambda (x) x))


(defun printl (&rest args)
    (map print (cons (elements args) args)))

;; (funcall printl (cons "738468736" (list "734678264!")))

(defmacro defgeneric (function-name)
    `(defun ,function-name (object &rest args)
        (funcall (get-method object ',function-name) (cons object args))))

(defmacro defmethod (class method-name signature body)
    `(set-attr ,class ',method-name
        (lambda ,(cons 'self signature)
            ,body)))

(defmethod standard-object print-l  (x &rest args)
        (progn 
            (print "print-l-method")
            (print self)
            (print x)
            (print args)))

(defgeneric make-instance)

(defmethod standard-object make-instance ()
    (progn 
        (defclass *unnamed-instance* self)
        (set-attr *unnamed-instance* '*TYPE* *INSTANCE-TYPE*)
        *unnamed-instance*))

(make-instance standard-object)
