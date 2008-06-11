;;
;;  DSL for GUI definitions.
;;

(defvar *widget-counter* 0)

(defmacro init-widget (widget init-forms &rest subwidgets)
    `(let ((w ,widget))
        (eval (cons 'w/object (cons 'w ',init-forms)))
        (map 
            (lambda (subwidget) (add w (eval subwidget)))
            ',subwidgets)
        w))

(defmacro button (init-forms)
    `(init-widget
        (w/object (make-instance C/BUTTON)
            (set-id (tostring (incput *widget-counter*)))
            (set-font *ui-button-font*)
            (set-size 40 20))
        ,init-forms))

(defun test-init-widget ()
    (add *top*
        (init-widget (make-instance C/CONTAINER)
            ((set-size 100 100) (set-position 10 10))
            (init-widget (make-instance C/LABEL)
                ((set-caption "The Fig is work")
                (set-font *ui-button-font*)
                (set-size 100 100))))))

(defun test-button ()
    (show-widget
        (button
            ((set-caption "Some wonky name")
             (set-size 100 20)))))