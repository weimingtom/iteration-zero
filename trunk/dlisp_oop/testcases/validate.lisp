#! /usr/bin/env dle

(print "Running dLISP test cases:" *ln*)
(set fn function)

(defmacro test (message &rest forms)
  `(progn
     (print "Testing " ,message *ln*)
     (map eval ',forms)))

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
(test "SET"
  (check-equal 3 (set a 1 b 2 c 3))
  (check-equal '(1 2 3) (list a b c))
  (check-equal 7 (set a (1+ b) b (1+ a) c (+ a b)))
  (check-equal '(3 4 7) (list a b c)))

;;;
;;; PUT
(test "PUT"
  (check-equal '(1 2 3)
	       (set x (cons 'a 'b) y (list 1 2 3)))
  (check-equal '(1 X 3)
	       (put (first x) 'x (first (rest y)) (first x) (rest x) y))
  (check-equal '(X 1 X 3) X)
  (check-equal '(1 X 3) y))

;;;
;;; QUOTE
(test "QUOTE"
 (check-equal 'a (quote a))
 (check-equal 1 (set a 1))
 (check-equal '(SETQ A 3) (quote (setq a 3)))
 (check-equal 'a (first '(a b))))

;;;
;;; LAMBDA
(test "LAMBDA"
  (check-equal 7
	       ((lambda (x) (+ x 3)) 4))
  (check-equal "foo"
	       ((lambda () "foo"))))

;;;
;;; EVAL
(test "EVAL"
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
(test "PARSE"
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
(test "DEFUN"
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
(test "CLOSURES"
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
(test "DEFMACRO"
      (defmacro inc (x) (list 'setq x (list '1+ x)))
      (set a 41)
      (check-equal 42
		   (inc a))
      (check-equal 42
		   a))

;;;
;;; LOOPCONTROL
(test "DOTIMES"
  (check-equal 9
	       (dotimes (k 10) k)))


;;;
;;; CREATE-OBJECT
(test "CREATE-OBJECT"
      (set name (create-object))
      (set-attr name 'attr1 1)
      (check-equal (get-attr name 'attr1) 1)
      (set-attr name 'inc-attr1
		(lambda (self) 
		  (set-attr self 'attr1 (1+ (get-attr name 'attr1)))))
      ((gencall inc-attr1) name)
      (check-equal 2
		   (get-attr name 'attr1)))

(println
 "Known to be broken are self-made generic functions.") 

