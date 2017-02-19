(declare-variable s1 String)
(declare-variable i1 Int)
(declare-variable i2 Int)


(assert (= s1 "abc"))

(assert (= i1 (Length s1)))

(assert (= i2 i1))

(check-sat)
(get-model)