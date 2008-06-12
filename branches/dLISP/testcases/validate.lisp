#! /usr/bin/env dle

(print "Running dLISP test cases:" *ln*)
(set fn function)

(defmacro make-check (pred &rest forms)
  `(fn (value)
       (let ((rval (apply progn ,forms)))
	 (if (,pred ,value rval)
	   (print "OK:  " ,value " <=> " ,forms *ln*)
	   (print "ERR: " ,value " <=/=> " ,forms *ln*)))))

(defmacro check-eq (value form)
  `(let ((rval ,form))
     (unless (eq ,value rval)
	 (print "ERR: " ,value " <=/=> " rval *ln*))))

(defmacro check-equal (value form)
  `(let ((rval ,form))
     (unless (equal ,value rval)
	 (print "ERR: expected " ,value " <=/=> " rval "  from " ',form *ln*))))

;; Check the ckecks
(check-eq 1 (+ 6 -5))

;;;
;;; SET
(progn
  (check-equal 3 (set a 1 b 2 c 3))
  (check-equal '(1 2 3) (list a b c))
  (check-equal 7 (set a (1+ b) b (1+ a) c (+ a b)))
  (check-equal '(3 4 7) (list a b c)))

;;;
;;; PUT
(progn
  (check-equal '(1 2 3)
	       (set x (cons 'a 'b) y (list 1 2 3)))
  (check-equal '(1 X 3)
	       (put (first x) 'x (first (rest y)) (first x) (rest x) y))
  (check-equal '(X 1 X 3) X)
  (check-equal '(1 X 3) y))

;;;
;;; QUOTE
(progn
 (check-equal 'a (quote a))
 (check-equal 1 (set a 1))
 (check-equal '(SETQ A 3) (quote (setq a 3)))
 (check-equal 'a (first '(a b))))

;;;
;;; LAMBDA
(progn
  (check-equal 7
	       ((lambda (x) (+ x 3)) 4))
  (check-equal "foo"
	       ((lambda () "foo"))))

;;;
;;; EVAL
(progn
  (check-equal 999
	       (setq form '(1+ a) a 999))
  (check-equal 1000
	       (eval form))
  (check-equal '(1+ A)
	       (eval 'form))
  (check-equal 42
	       (let ((a 41)) (eval form))))

;;;
;;; PARSE
(progn
  (check-equal '(+ 1 2)
	       (set foo (parse "(+ 1 2)")))
  (check-equal 3
	       (eval foo))
  (check-equal 3
	       (eval (parse "(+ 1 2)")))
  (check-equal 9
	       (eval (parse (open "testcases/validateparse.lisp")))))

;;;
;;; DEFUN
(progn
  (set plus1 (lambda (b) (+ 1 b)))
  (defun ortest (a &optional (b 2) &rest c) (list a b c))
  (check-equal '(1 2 NIL)
	       (ortest 1))
  (check-equal '(1 3 (4))
	       (ortest 1 (1+ 2) (plus1 3)))
  (check-equal '(1 2 (3 4))
	       (ortest 1 2 3 4)))

;;
;; CLOSURES
(progn (print "CLOSURES" *ln*)
  (defun make-counter ()
    (let ((x 0))
      (lambda () (set x (+ x 1)) x)))
  (set f1 (make-counter))
  (set f2 (make-counter))
  (check-eq 1 (f1))
  (check-eq 2 (f1))
  (check-eq 3 (f1))
  (check-eq 1 (f2))
  (check-eq 4 (f1))
  (check-eq 2 (f2)))

;;;
;;; DEFMACRO
 (print "DEFMACRO" *ln*)
(if (defmacro inc (x) (list 'setq x (list '1+ x))) "OK defmacro1" "NOK defmacro1")
(if (equal (setq a 41) 41) "OK defmacro2" "NOK defmacro2") 
(if (equal (inc a) 42) "OK defmacro3" "NOK defmacro3")
(if (equal a 42) "OK defmacro4" "NOK defmacro4")

;;;
;;; LOOPCONTROL
(progn
  (check-equal 9
	       (dotimes (k 10) k)))


;;;
;;; CREATE-OBJECT
(let ((name (create-object)))
    (set-attr name 'attr1 1)
    (print 
        (if (equal (get-attr name 'attr1) 1)  "OK set/get-attr" "NOK set/get-attr"))
    (set-attr name 'inc-attr1
        (lambda (self) 
            (set-attr self 'attr1 (1+ (get-attr name 'attr1)))))
    (inc-attr1 name)
    (if (equal (get-attr name 'attr1) 2)  "OK set/get-attr2" "NOK set/get-attr2"))

(let ((x 0))
    (dotimes (k 10)
        (set x (1+ x)))
    (if (equal x 10)
        "OK dotimes" "NOK dotimes"))

;;; RETURN LAST ELEMENT
