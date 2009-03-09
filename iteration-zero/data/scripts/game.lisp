(defun get-level-state ()
    (get-state *engine* "level-state"))

(defun load-and-start-level (filename)
    (load (get-level-state) filename)
    (start *engine* "level-state"))

(on-start (get-state *engine* "level-state")
    (lambda ()
        (let ((portraits (make-party-portraits-widget *party*)))
            (show-widget portraits)
            (on-stop (get-level-state) (bind1st hide-widget portraits)))))

