(declare-variable s String)
(declare-variable var String)
(declare-variable ret String)


(assert (ite  (or (Contains s "<") (Contains s ">") )
              (= ret "x")
              (= ret s)
        ) 
)


(get-model)