;;
;; Startup of the game iteration zero.
;;

(load "data/scripts/basics.lisp" t)
(load "data/scripts/layout.lisp" t)
(load "data/scripts/ui.lisp" t)

(defvar *main-menu*
    '(("New Party"    (start *engine* "new-party-state"))
      ("Demo Level01" (load-and-start-level "data/test/level01.dl"))
      ("Demo Level02" (load-and-start-level "data/test/level02.dl"))
      ("Quit"         (stop *engine*))))

(defvar *toolbar*
    '(("quit" (stop *engine*))
     ("menu" (start *engine* "menu"))))

(defmacro menu-option (item options) `(eval (find ,item ,options)))
(defvar *new-party-menu-left*
    '(("Race"  (menu-option 'race *character-creation-steps*))
      ("Class" (menu-option 'race *character-creation-steps*))
      ("Name"  (menu-option 'race *character-creation-steps*))
      (0       (not-implemented))
      ("Add"   (not-implemented))
      ("Back"  (start *engine* "menu"))))

(defvar *character-creation-steps*
    '((race (show-race-selection))))

(defun load-and-start-level (filename)
    (load (get-state *engine* "level-state") filename)
    (start *engine* "level-state"))

(defmacro bind1st (fun a)
    `(lambda (&rest args)
        (funcall ,fun (cons ,a args))))

(defun make-main-menu ()
    (center-on-screen
        (create-menu *main-menu* "main-menu"
            (/ (x-resolution *engine*) 2)
            (/ (y-resolution *engine*) 2))))

(add-state *engine*
    (let ((menu-state (make-instance C/GAME-STATE "menu"))
          (main-menu (make-main-menu)))
        (on-start menu-state (lambda () (show-widget main-menu)))
        (on-stop  menu-state  (lambda () (hide-widget main-menu)))
        menu-state))

(defun character-selected (chara)
    (print "Character selected " (get-name chara) *ln*)
    (let ((n (index chara  (get-members *party*))))
        (when n
            (print "Index is: " n *ln*)
            (set-active *party* n))))

(defun make-character-portrait (character)
    (let ((button (make-instance C/BUTTON)))
        (set-size button 100 100)
        (set-caption button (get-name character))
        (on-mouse-clicked button (lambda (event) (character-selected character)))))

(defun make-range (start &optional (stop nil))
    (if (eq stop nil)
        (make-range 0 start)
        (if (>= start stop) ()
            (cons start (make-range (inc start) stop)))))

(defun get-members (party)
    (map (lambda (i) (get party i)) (make-range 1 (1+ (get-size party)))))

(defun make-party-portraits-widget (party)
    (let ((box (make-instance C/CONTAINER))
          (party-members (get-members party))
          (portraits ()))
        (print "party-members " (map get-name party-members) *ln*)
        (set portraits (map make-character-portrait party-members))
        (w/object box
            (hpack-widgets 0 0 portraits)
            (set-size (- (x-resolution *engine*) 200) 100)
            (set-position 20 20)
            (set-opaque nil))))

(defun make-create-party-interface ()
    (let ((left-menu (create-menu *new-party-menu-left* "new-party-menu-left" 200))
          (portraits ()))
        (set portraits (make-party-portraits-widget *party*))
        (set-position left-menu 20 140)
        (list left-menu portraits)))

(defvar *party* (make-instance C/PARTY))

(defun start-party-creation ()
    (put *party* (make-instance c/party))
    (add *party* (make-instance c/character "some punk"))
    (let ((party-widgets (make-create-party-interface)))
        (map show-widget party-widgets)
        (on-stop (get-state *engine* "new-party-state")
            (lambda () (map hide-widget party-widgets)))))

(add-state *engine*
    (let ((new-party-state (make-instance C/GAME-STATE "new-party-state")))
        (on-start new-party-state start-party-creation)
         new-party-state))

(on-start (get-state *engine* "level-state")
    (lambda ()
        (let ((portraits (make-party-portraits-widget *party*)))
            (show-widget portraits)
            (on-stop (get-state *engine* "level-state") (bind1st hide-widget portraits)))))

;;(show-quit-button)


(show-widget
    (w/object (create-toolbar *toolbar*)
        (set-x 5)
        (set-y (- (y-resolution *engine*) 25))))

(test-button)
(start *engine* "menu")


