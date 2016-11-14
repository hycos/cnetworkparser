(declare-variable s1 String)
(declare-variable s2 String)

(assert (= s1 "/*<![CDATA[*/" ))
(assert (= s2 "\\" ))
(assert (= s2 "~" ))

(check-sat)
(get-model)
