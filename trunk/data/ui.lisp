;;
;; Ui Helpers
;;

(defvar *gui* (get-gui *engine*))
(defvar *top* (cast C/CONTAINER (get-top *gui*)))

(defvar *ui-button-font* (make-instance C/GL-FONT "data/fonts/7service/7the.ttf" 14))
(defvar *ui-text-font*   (make-instance C/GL-FONT "data/fonts/vera/Vera.ttf" 12))

(defun center-in (inset w)
    (/ (- w inset) 2))

(defun center-widget (widget w h)
    (progn
        (set-position widget
            (/ (- w (get-width widget)) 2)
            (/ (- h (get-height widget)) 2))
        widget))

(defun center-on-screen (widget)
    (center-widget widget
        (x-resolution *engine*)
        (y-resolution *engine*)))

(defun not-implemented ()
    (show-widget (make-info-window "not implemented")))

(defun vpack-widgets (box starty startx widgets)
    (let ((maxw (funcall max (map get-width widgets)))
          (maxh (funcall max (map get-height widgets)))
          (y starty))
        (dolist (widget widgets)
            (set-position widget startx y)
            (set-size widget maxw maxh)
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
            (set x (+ x maxh 5)))
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
            (remove *top* root))))

(defun make-close-handler ()
    (lambda (event)
        (hide-widget (get-source event))))

(defun create-button (name w h onclick)
    (let ((button (make-instance C/BUTTON)))
            (set-position button 0 0)
            (set-size button w h)
            (set-caption button name)
            (on-mouse-clicked button onclick)
            button))

(defun create-menu-button (option box w)
    (if (not (first option))
        (make-instance C/CONTAINER)
        (create-button (first option) w 30
            (lambda (event)
                (eval (second option))))))

(defun create-menu (options &optional (name "unnamed") (width 200) (h 0))
    (let ((box (make-instance C/CONTAINER))
          (y 0)
          (make-button
            (lambda (o) (create-menu-button o box width))))
        (set-id box name)
        (set-size box width h)
;;         (set-border-size 2)
        (set-opaque box nil)
        (when (<= h 0)
            (set-height box (+ (* (length options) 35) 10)))
        (vpack-widgets box 0 0 (map make-button options))
        box))

(defun make-info-window (text)
    (let ((win (make-instance C/WINDOW))
          (label (make-instance C/LABEL))
          (close-button (make-instance C/BUTTON))
          (w 300) (h 200))
        (set-size win (+ w 25) (+ h 25))
        (set-caption win "Info")
        (set-caption label text)
        (set-font label *ui-text-font*)
        (set-caption close-button "X")
        (set-size close-button 40 40)
        (set-size label (- w 20) (- h (get-height close-button)))
        (on-mouse-clicked close-button (make-close-handler))
        (center-widget label w h)
        (set-position close-button (- w (get-width close-button)) (- h (get-height close-button) 10)) 
        (add win label)
        (add win close-button)
        (center-widget win (x-resolution *engine*) (y-resolution *engine*))
        (on-widget-shown win (lambda (event) (request-modal-focus win)))
        win))