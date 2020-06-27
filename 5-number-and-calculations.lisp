(defun compute-ln (&optional &key (x 0) (n 1))
    (cond
        ((or (not (numberp x)) (not (numberp n))) (format t "~a" "Not a number."))
        ((minusp n) (format t "~a" "Please enter a positive integer"))
        ((or (< x -1) (> x 1)) (format t "~a" "X is outside of the range."))
        (t (maclaurin-series x n))
    )
)

(defun power (x y)
    (if (zerop y) 1
    (* x (power x (- y 1))))
)

(defun maclaurin (x n)
    (/ (* (power -1 (+ n 1)) (power x n)) n)
)

(defun maclaurin-series (x n)
    (cond ((= n 1) x)
          (t (+ (maclaurin x n) (maclaurin-series x (- n 1))))  
    ))

;; (compute-ln)
;; (compute-ln :x 'a :n 'b)
(compute-ln :x 3 :n 5)
;; (compute-ln :x -2 :n 5)
;; (compute-ln :x 1 :n 5)
;; (compute-ln :x 1 :n 3)