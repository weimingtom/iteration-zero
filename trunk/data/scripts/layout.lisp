;;
;;  DSL for GUI definitions.
;;

(defvar *widget-counter* 0)

(defun no-func (&rest args) nil)

(defmacro init-widget (widget init-forms &optional (packer no-func) (subwidgets nil))
    (let ((w (gensym "init-widget")))
        `(let ((,w ,widget))
            (apply w/object (cons ,w ,init-forms))
            (,packer ,w ,subwidgets)
            ,w)))

(defun vpack-widgets (box starty startx widgets)
    (let ((maxw (funcall max (map (<- get-width) widgets)))
          (maxh (funcall max (map (<- get-height) widgets)))
          (y starty))
        (dolist (widget widgets)
            (set-position widget startx y)
            (set-size widget maxw maxh)
            (print add box widget *ln*)
            (add box widget)
            (set y (+ y maxh 5)))
        box))

(defun hpack-widgets (box starty startx widgets)
    (let ((maxw (funcall max (map get-width widgets)))
          (maxh (funcall max (map get-height widgets)))
          (x startx))
        (dolist (widget widgets)
            (set-position widget x starty)
            (set-size widget maxw maxh)
            (add box widget)
            (set x (+ x maxw 5)))
        box))

(defmacro button (init-forms)
    `(init-widget
        (w/object (make-instance C/BUTTON)
            (set-id (tostring (incput *widget-counter*)))
            (set-font *ui-button-font*)
            (set-position 0 0)
            (set-size 40 20))
         ',init-forms))

(defun vbox-pack (box widgets)
    (print "vboxbapck:" widgets *ln*)
    (vpack-widgets box 0 0 widgets)
    (when widgets
        (set-height box 
            (* (length widgets) (+ 5 (apply max (map (<- get-height) widgets)))))
        (set-width  box 
            (+ 5 (apply max (map (<- get-width)  widgets))))))

(defun prx (x) (print "PRX: " x *ln*) x)

(defmacro vbox (init-forms &rest subwidgets)
    `(init-widget
        (w/object (make-instance C/CONTAINER)
            (set-id (tostring (incput *widget-counter*)))
            (set-position 500 50)
            (set-size 40 20))
        ',init-forms
        vbox-pack (map eval ',subwidgets)))

;;
;; Example of the planned UI definition.
;;

(defun test-button ()
    (print "TTTTTTTTTT" *ln*)
    (show-widget
        (vbox ((set-position 300 50))
            (button
                ((set-caption "Some wonky name")
                (set-size 400 40)
                (print "<<<<<<<<<<<<<" *ln*)))
            (button
                ((set-caption "Some wonky name")
                (print "<<<<<<<<<<<<<" *ln*)
                (set-size 400 40))))

))
