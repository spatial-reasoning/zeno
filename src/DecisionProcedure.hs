module DecisionProcedure where
--fixme: split this module!

-- standard modules

-- local modules
import Basics

firstApply fun dp = dp{ decProProc = decProProc dp . fun }

data DecisionProcedure a = DecisionProcedure
        { decProName :: String
        , decProProc :: Network [String] a -> Maybe Bool
        }

class HasDecisionProcedure a where
    procedures :: a -> [ DecisionProcedure a ]

