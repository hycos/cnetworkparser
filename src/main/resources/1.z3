(declare-variable s1 String)
(declare-variable s2 String)

(assert (= true (StartsWith s1 s2) ) )
(assert (RegexIn s2 (RegexUnion (Str2Reg "id") (Str2Reg "name"))))
(assert (RegexIn s1 (RegexConcat (RegexConcat (RegexStar (RegexCharRange "!" "~")) (Str2Reg "<")) (RegexStar (RegexCharRange "!" "~")) ) ))

(check-sat)
(get-model)
