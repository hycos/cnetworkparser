(declare-variable filename_1 String)
(declare-variable filename_2 String)
(declare-variable i3 Int)



(assert (= i3 (Indexof filename_1 ".") ) )

(assert (= filename_2 (Substring filename_1 0 i3) ))

(assert (Contains filename_2 "../") )


(check-sat)
(get-model)
