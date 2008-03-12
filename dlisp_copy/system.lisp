
(defun 1+ (a) 
    "(1+ <num>); return <num> increased by 1."
  (+ a 1))

(defun 1- (a) 
    "(1- <num>); return <num> decreased by 1."
  (- a 1))

(defun find (needle haystack)
  (car (member needle haystack)))

(defun sublist (index count list)
    "(SUBLIST <index> <count> <list>); Copy list from <index> for <count> items."
  (copy count (from index list)))

(defun print (&rest objects)
  (dolist (object objects)
    (write *std-out* object)))

(defun println (&rest objects)
  (progn
    (dolist (object objects)
      (write *std-out* (join (tostring object) *ln*)))
    (first (last objects))))

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

(defmacro defvar (name &optional initial-value)
  `(unless (isbound ,name) (set ,name ,initial-value)))

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

