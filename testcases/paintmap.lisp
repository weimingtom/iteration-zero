;; Takes a (name . colour) country-concell and returns true if colour is set.
(defun has-colour (country)
  (not (eq (rest country) nil)))

;; Takes a state and returns true if all countries in it has colour set
(defun is-goal (state)
    (has-colour (first (last state))))

;; Returns a score for estimated proximity to goal for a state, low values are good.
(defun estimate (state)
    (if (isempty state)
            0
        (+ (if (has-colour (first state)) 0 1) (estimate (rest state)))))

;; Returns the first (name . colour) country-concell that has no colour set.
(defun first-colourless (state)
    (cond
        ((not (has-colour (first state))) (first state))
        (T (first-colourless (rest state)))))

;; Takes a list of neighboring countries and search a state to see if any of
;; them has the given colour, returns true if so.
(defun neighbors-has-colour (state neighbors colour)
    (if (isempty neighbors)
            nil
        (or
            (eq (rest (assoc (first neighbors) state)) colour)
            (neighbors-has-colour state (rest neighbors) colour))))

;; Returns true if any of the neighbors to the given country has the specified colour.
(defun neighbor-has-colour (state country colour)
    (neighbors-has-colour state (assoc (car country) map) colour))

;; Returns a new state where the colour is set for the specified country in the given state.
(defun set-colour (state country colour)
    (if (isempty state)
            '()
        (append
            (list
                (if (equal (first state) country)
                        (cons (car (first state)) colour)
                    (first state)))
            (set-colour (rest state) country colour))))

;; Return a new state if giving the colour to the state is an legal action.
(defun legal-successor (state colour)
    (let ((first-colourless_state (first-colourless state)))
        (if (neighbor-has-colour state first-colourless_state colour)
                '()
            (list (set-colour state first-colourless_state colour)))))

;; Return a list of possible successor states for the given state, nil if none are available.
;; (defun successor (state &optional (colour-list colours))
(defun successor (state colour-list)
    (if (isempty colour-list)
            '()
        (append (legal-successor state (first colour-list)) (successor state (rest colour-list)))))

;; Depth-first-search algoritm. Provided by Amy, RUN THIS!
(defun dfs (states)
    (cond 
        ((null states) nil)
        ((is-goal (first states)) (first states))
        (t (dfs (append (successor (first states) colours)
                (rest states))))))

