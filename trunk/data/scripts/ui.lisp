;;
;; GUI Helpers
;;

;; Top Level Globals
(defvar *gui* (get-gui *engine*))
(defvar *top* (cast C/CONTAINER (get-top *gui*)))

;; Some fonts
(defvar *ui-button-font* (make-instance C/GL-FONT "data/fonts/7service/7the.ttf" 14))
(defvar *ui-text-font*   (make-instance C/GL-FONT "data/fonts/vera/Vera.ttf" 12))

;; Geometry
(defun center-in (inset w)
    (/ (- w inset) 2))

(defun center-widget (widget w h)
    (set-position widget
        (/ (- w (get-width widget)) 2)
        (/ (- h (get-height widget)) 2))
    widget)

(defun center-on-screen (widget)
    (center-widget widget
        (x-resolution *engine*)
        (y-resolution *engine*)))

(defun not-implemented ()
    (show-widget (make-info-window "not implemented")))


(defun show-widget (w)
    (add *top* w) 
    w)

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

(defun create-menu-button (option box w &optional (h 30))
    (if (not (first option))
        (make-instance C/CONTAINER)
        (create-button (first option) w 30
            (lambda (event)
                (eval (second option))))))

(defun create-menu (options &optional (name "unnamed") (width 200) (h 0))
    (let ((box (make-instance C/CONTAINER))
          (y 0))
        (set make-button (lambda (o) (print y) (create-menu-button o box width)))
        (set-id box name)
        (set-size box width h)
;;         (set-border-size 2)
        (set-opaque box nil)
        (when (<= h 0)
            (set-height box (+ (* (length options) 35) 10)))
        (vpack-widgets box 0 0 (map make-button options))
        box))

(defun create-toolbar (options)
    (let ((box (make-instance C/CONTAINER)))
        (let ((x 0)
            (make-button
                (lambda (o) (create-menu-button o box 20 20 )))
            (align-button (lambda (button)
                    (set-font button *ui-text-font*)
                    (set-size button 
                        (+ 10 (get-width *ui-text-font* (get-caption button)))
                        (+ 5  (get-height *ui-text-font*)))
                    button)))
        (set-id box "toolbar")
        (set-opaque box nil)
        (let ((buttons (map align-button (map make-button options))))
            (set-size box (+ (* (length options) (+ 5 (funcall max (map get-width buttons) 10)))) 20)
            (hpack-widgets box 0 0 buttons))
        box)))
    

(defun make-info-window (text)
    (let ((win (make-instance C/WINDOW))
          (label (make-instance C/LABEL))
          (close-button (make-instance C/BUTTON))
          (w 300) (h 200))
        (set-size win (+ w 25) (+ h 25))
        (set-font win *ui-text-font*)
        (set-caption win "Info")
        (set-caption label text)
        (set-font label *ui-text-font*)
        (set-font close-button *ui-text-font*)
        (set-caption close-button "Close")
        (set-size close-button (+ (get-width *ui-text-font* (get-caption close-button)) 20) 30)
        (set-size label (- w 20) (- h (get-height close-button)))
        (on-mouse-clicked close-button (make-close-handler))
        (center-widget label w h)
        (set-position close-button (- w (get-width close-button) -10) (- h (get-height close-button) 10)) 
        (add win label)
        (add win close-button)
        (center-widget win (x-resolution *engine*) (y-resolution *engine*))
        (on-widget-shown win (lambda (event) (request-modal-focus win)))
        win))




