(declare-fun x () Int)
(declare-fun y () Int)

(assert (= x y))
(assert (= y (+ x 7)))

(check-sat)