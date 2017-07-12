module CAS.Dumb.Tree where


data CAS' γ s² s¹ s⁰ = Symbol !s⁰

infixr 4 :=:
data Equality' γ s² s¹ s⁰
  = (:=:) { originalExpression :: !(CAS' γ s² s¹ s⁰)
          , transformedExpression :: !(CAS' γ s² s¹ s⁰) }

