(declare-variable filename_1 String)
(declare-variable filename_2 String)
(declare-variable i3 Int)



(assert (= i3 (Indexof filename_1 ".") ) )



(check-sat)
(get-model)
