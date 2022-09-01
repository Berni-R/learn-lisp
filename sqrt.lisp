(defun try (guess x eps)
    (if (good-enough? guess x eps)
        guess
        (try (improve guess x) x eps)
    )
)

(defun m-sqrt (x &optional (eps 1d-12))
    (let ((xd (coerce x 'double-float)))
        (try 1.0 xd eps)
    )
)

(defun improve (guess x)
    (/ (+ guess (/ x guess)) 2)
)

(defun good-enough? (guess x eps)
    (< (abs (- 1 (/ (* guess guess) x))) eps)
)

