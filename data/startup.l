(defvar *gui* (get-gui *engine*))
(defvar *top* (cast container (get-top *gui*)))

(defun pack-widgets-vertically (box starty startx widgets)
    (let ((maxw (funcall max (collect (gencall get-width) widgets)))
          (maxh (funcall max (collect (gencall get-height) widgets)))
          (y starty))
        (dolist (widget widgets)
            (set-position widget startx y)
            (set-size widget maxw maxh)
            (add box widget)
            (incput y (+ maxh 5)))
        box))

(defun show-widget (w)
    (progn (add *top* w) w))

(defun show-quit-button ()
    (show-widget
        (let ((quit-button (create-button "quit" 100 40 (lambda (ev) (stop *engine*)))))
            (set-position quit-button (- (x-resolution *engine*) 105) 5)
            quit-button)))

(defun get-widget-root (widget)
    (if (get-parent widget)
        (if (get-parent (get-parent widget))
            (get-widget-root (get-parent widget))
            widget)
        nil))

(defun hide-widget (widget)
    (let ((root (get-widget-root widget)))
        (when root 
            ;;(print "ROOT:" root  (get-id root) *ln*)
            (remove *top* root))))

(defun make-close-handler ()
    (lambda (event)
        (hide-widget (get-source event))))

(defun create-button (name w h onclick)
    (let ((b (make-instance button)))
            (set-position b 0 0)
            (set-size b w h)
            (set-caption b name)
            (on-mouse-clicked b onclick)
            b))

(defun create-menu-button (name filename c w)
    (create-button name w 30
        (lambda (event)
            (load (get-state *engine* "test") filename)
            (hide-widget (get-source event))
            (start *engine* "test"))))

(defun create-menu (options)
    (let ((box (make-instance container))
          (x4 (/ (x-resolution *engine*) 4))
          (y4 (/ (y-resolution *engine*) 4))
          (y 0)
          (make-button
            (lambda (option) (create-menu-button (first option) (second option) box (* 2 x4)))))
        (set-id box "main-menu")
        (set-size box (* 2 x4) (* 2 y4))
        (set-position box x4 y4)
        (set-opaque box nil)
        (pack-widgets-vertically box 0 0 (map make-button options))
        box))





(show-widget
    (create-menu '(("Level02" "data/test/level02.dl") ("Level01" "data/test/level01.dl"))))

(show-quit-button)
