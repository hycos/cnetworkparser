(declare-fun x () Int)
(declare-fun y () Int)

(assert (= x (+ y 3)))
(assert (= y (+ x 7)))

(check-sat)