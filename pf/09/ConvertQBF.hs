import qualified QBF       as QBF
import qualified NestedQBF as NestedQBF

scopeCheck :: (QBF.Var -> Maybe v) -> QBF.Formula -> Maybe (NestedQBF.Formula v)
scopeCheck e QBF.Bot = pure NestedQBF.Bot
scopeCheck e (QBF.Var v) = NestedQBF.Var <$> e v
scopeCheck e (QBF.Not f) = NestedQBF.Not <$> scopeCheck e f
scopeCheck e (QBF.And f1 f2) =
    NestedQBF.And <$> scopeCheck e f1 <*> scopeCheck e f2
scopeCheck e (QBF.All b f) = NestedQBF.All <$> scopeCheck e' f
  where e' s = if s == b then pure NestedQBF.Z else NestedQBF.S <$> e s

