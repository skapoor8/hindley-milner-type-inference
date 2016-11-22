val _ = constraintTest (CONAPP (TYCON "list", [TYVAR "a"]) ~ TYVAR("b"))
val _ = constraintTest (TYCON "a" ~ TYVAR "a")
val _ = constraintTest (TYCON "bool" ~ TYVAR "a")