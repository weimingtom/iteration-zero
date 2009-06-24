;;   system.lisp
;;   dLISP
;; 
;;   Author: Fredrik Olsson <peylow@treyst.se>
;;   Author: Klaus Blindert <klaus.blindert@web.de>
;; 
;;   Copyright (c) 2005 Treyst AB, <http://www.treyst.se>
;;   Copyright (c) 2008 Klaus Blindert
;; 
;;   All rights reserved.
;; 
;;     This file is part of dLISP.
;;     dLISP is free software; you can redistribute it and/or modify
;;     it under the terms of the GNU Lesser General Public License as published by
;;     the Free Software Foundation; either version 2.1 of the License, or
;;     (at your option) any later version.
;; 
;;     dLISP is distributed in the hope that it will be useful,
;;     but WITHOUT ANY WARRANTY; without even the implied warranty of
;;     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;     GNU Lesser General Public License for more details.
;; 
;;     You should have received a copy of the GNU Lesser General Public License
;;     along with dLISP; if not, write to the Free Software
;;     Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA

(defmacro gencall (function-name)
     `(lambda (object &rest args)
         (funcall (get-method object ',function-name) (cons object args))))
(set <- gencall)

(defmacro call (function-name object &rest args)
   `(funcall (get-method ,object ',function-name) (cons ,object ,args)))
;; (set b (create-object))
;; (set-attr b 'print (lambda (self) (write *std-out* "Hello!" *ln*)))
;; (call print b)
(set . call)

(defmacro defvar (name &optional initial-value)
  `(unless (boundp ',name)
      (set! ,name ,initial-value)))

(defvar *gensym-counter* 0)
(defmacro map (fun l)
    (let ((stack (gensym)) (item (gensym)))
    `(let ((,stack '()))
        (dolist (,item ,l (reverse ,stack))
            (set ,stack (cons (,fun ,item) ,stack))))))

(defun gensym (&optional (prefix "gensym"))
    (symbol (join prefix ":" (tostring (incput *gensym-counter*)))))

(set! fn lambda)

(defun 1+ (a) 
    "(1+ <num>); return <num> increased by 1."
  (+ a 1))

(defun 1- (a) 
    "(1- <num>); return <num> decreased by 1."
  (- a 1))

(defun find (needle haystack)
  (cdr (assoc needle haystack)))

;; Ugliest index function in town.
(defun index (needle haystack)
    (if (eq haystack nil) nil
        (if (eq needle (first haystack)) 1
            (let ((subi (index needle (rest haystack))))
;;                 (print "index " needle " " haystack " " subi *ln*)
                (if (isnum subi) (1+ subi) subi)))))

(defun sublist (index count list)
    "(SUBLIST <index> <count> <list>); Copy list from <index> for <count> items."
  (copy count (from index list)))

(defun print (&rest objects)
  (dolist (object objects)
    (write *std-out* object)))

(defun println (&rest objects)
    (dolist (object objects)
      (write *std-out* (join (tostring object) *ln*)))
    (first (last objects)))

(defmacro inc (a &optional (d 1))
  `(+ ,a ,d))

(defmacro dec (a &optional (d 1))
  `(- ,a ,d))

(defmacro incput (a &optional (d 1))
  `(put ,a (+ ,a ,d)))
(defmacro decput (a &optional (d 1))
  `(put ,a (- ,a ,d)))

(defmacro return (&optional a)
  `(return-from nil ,a))


(defun second (a) (nth 1 a))
(defun third (a) (nth 2 a))
(defun fourth (a) (nth 3 a))
(defun fifth (a) (nth 4 a))
(defun sixth (a) (nth 5 a))
(defun seventh (a) (nth 6 a))
(defun eight (a) (nth 7 a))
(defun ninth (a) (nth 8 a))
(defun tenth (a) (nth 9 a))

(defun iszero (a) 
    "(ISZERO <num>); Returns true if <num> is zero."
  (= a 0))
(set zerop iszero)

(defun min (&rest values)
    "(MIN <num> ...); Returns the least of <num>s."
  (let ((min (first values))) 
    (dolist (a (rest values) min)
      (when (< a min) (set min a)))))

(defun max (&rest values) 
    "(MAX <num> ...); Returns the greatest of <num>s."
  (let ((max (first values))) 
    (dolist (a (rest values) max)
      (when (> a max) (set max a)))))

(defun avg (&rest values)
    "(AVG <num> ...); Returns the average of <num>s."
  (let ((avg 0))
    (dolist (a values (unless (isempty values) (/ avg (elements values))))
      (set avg (+ avg a)))))

(defun med (&rest values)
    "(MED <num> ...); Returns the median of <num>s."
  (let ((values (sort values <)) (num-values (elements values)))
    (if (iszero (mod num-values 2))
        (avg (nth (/ num-values 2) values) (nth (1- (/ num-values 2)) values))
      (nth (/ num-values 2) values))))

(defun abs (a)
    "(ABS <num>); Returns the absolute value is <num>."
  (if (< a 0)
      (- a)
    a))

(defun reverse (l)
    "(REVERSE <list>) Returns the reverse list."
    (set stack '())
    (dolist (item l stack)
        (set stack (cons (first l) stack))
        (set l (rest l))))

(set length elements)

;; (defmacro call-method (name object &rest args)
;;     `(let ((m (get-attr ,object ,name)))
;;         (funcall m (cons ,object ,args))))

;; (defun get-method (object name)
;;     (if (has-attr object name)
;;         (get-attr object name)
;;         (if (isobject (get-attr object 'superclass))
;;             (get-method (get-attr object 'superclass) name))))

;; (defmacro defgeneric (function-name)
;;     `(defun ,function-name (object &rest args)
;;         (funcall (get-method object ',function-name) (cons object args))))
;; 
;; (defmacro defmethod (class method-name signature body)
;;     `(set-attr ,class ',method-name
;;         (lambda ,(cons 'self signature)
;;             ,body)))
;; 
