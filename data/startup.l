(defun get-top-container ()
    (let ((top-widget (get-top *gui*)))
        (print top-widget)
        (cast container top-widget)))

(set *gui* (get-gui *engine*))
(set *top* (get-top-container))


(set-logic-handler *engine*
    (lambda (f)
        (print "Logic handlers work!" *ln*)
        (set-logic-handler *engine* nil)))


(add-option *menu*
    (make-instance menu-option "Test-Level"))

(defun add-button (name x y)
    (let ((b (make-instance button)))
        (set-position b x y)
        (set-size b 100 200)
        (set-caption b name)
        (on-mouse-entered b
            (lambda (event) 
                (set-size b 200 100)))
        (on-mouse-exited b
            (lambda (event) 
                (set-size b 100 200)))
        (add *top* b)))

(add-button "HELLO" 10 10)