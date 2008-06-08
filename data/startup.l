(load "data/ui.lisp" t)

(defvar *main-menu*
    '(("New Party"    (start *engine* "new-party-state"))
      ("Demo Level01" (load-and-start-level "data/test/level01.dl"))
      ("Demo Level02" (load-and-start-level "data/test/level02.dl"))
      ("Quit"         (stop *engine*))))

(defmacro menu-option (item options) `(eval (find ,item ,options)))
(defvar *new-party-menu-left*
    '(("Race"  (menu-option 'race *character-creation-steps*))
      ("Class" (menu-option 'race *character-creation-steps*))
      ("Name"  (menu-option 'race *character-creation-steps*))
      (0       (not-implemented))
      ("Add"   (not-implemented))
      ("Back"  (not-implemented))))

(defvar *character-creation-steps*
    '((race (show-race-selection))))



(defun load-and-start-level (filename)
    (progn
        (load (get-state *engine* "level-state") filename)
        (start *engine* "level-state")))

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

(defun make-character-portrait (character)
    (let ((button (make-instance C/BUTTON)))
        (set-size button 100 100)
        (set-caption button (get-name character))
        (print (get-name character) *ln*)
        button))

(defun make-range (a &optional (b nil))
    (if (eq b nil)
        (make-range 0 a)
        (if (>= a b) ()
            (cons a (make-range (inc a) b)))))

(defun get-members (party)
    (map (lambda (i) (print "IIIIIIIIII " i *ln*) (get party i)) (make-range 1 (1+ (get-size party)))))

(defun make-party-portraits-widget (party)
    (let ((box (make-instance C/CONTAINER))
          (party-members (get-members party))
          (portraits ()))
        (print "party-members " party-members *ln*)
        (set portraits (map make-character-portrait party-members))
        (hpack-widgets box 0 0 portraits)
        (set-size box 200 100)
        (set-position box 20 20)
        (set-opaque box nil)
        box))

(defun make-create-party-interface ()
    (let ((left-menu (create-menu *new-party-menu-left* "new-party-menu-left" 200))
          (portraits ()))
        (put *party* (make-instance C/PARTY))
        (add *party* (make-instance C/CHARACTER "Some Punk"))
        (set portraits (make-party-portraits-widget *party*))
        (set-position left-menu 20 140)
        (list left-menu portraits)))

(defvar *party* (make-instance C/PARTY))

(add-state *engine*
    (let ((new-party-state (make-instance C/GAME-STATE "new-party-state")))
        (on-start new-party-state (lambda () (map show-widget (make-create-party-interface))))
        new-party-state))

(show-quit-button)
(start *engine* "menu")


