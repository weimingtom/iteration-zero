(defun get-top-container ()
    (let ((top-widget (get-top *gui*)))
        (cast container top-widget)))

(set *gui* (get-gui *engine*))
(set *top* (get-top-container))


(defun create-button (name w h onclick)
    (let ((b (make-instance button)))
            (print b)
            (set-position b 0 0)
            (set-size b w h)
            (set-caption b name)
            (on-mouse-clicked b onclick)
            b))

(defun create-menu-button (name filename c w)
    (let ((lstate (get-state *engine* "test")))
        (create-button name w 30
            (lambda (event) 
                (load lstate (second option))
                (remove *top* c)
                (start *engine* "test")))))

(defun create-menu (options)
    (let ((c (make-instance container))
          (x4 (/ (x-resolution *engine*) 4))
          (y4 (/ (y-resolution *engine*) 4))
          (y 0))
        (set-size c (* 2 x4) (* 2 y4))
        (set-position c x4 y4)
        (set-opaque c nil)
        (dolist (option options)
            (print (first option) " " (second option) *ln*)
            (let ((button (create-menu-button (first option) (second option) c (* 2 x4))))
                (set-position button 0 y)
                (add c button)
                (incput y 40)))
        c))

(add *top* 
    (create-menu '(("Level02" "data/test/level02.dl") ("Level01" "data/test/level01.dl"))))
