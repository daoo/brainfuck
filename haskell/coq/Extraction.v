Require Import Expr ssreflect ssrint.
Extraction Language Haskell.

Extract Inductive unit => "()" [ "()" ].
Extract Inductive bool => "Bool" [ "True" "False" ].
Extract Inductive sumbool => "Bool" [ "True" "False" ].
Extract Inductive list => "List" [ "[]" "(:)" ].
Extract Inductive option => "Maybe" [ "Just" "Nothing" ].
Extract Inductive prod => "(,)" [ "(,)" ].
Extract Inductive int => "Int" ["id" "-"]
  "(\fPos fNeg n -> if n >= 0 then fPos n else fNeg n)".

Extract Inductive Datatypes.nat => "Int" ["0" "succ"]
  "(\fO fS n -> if n==0 then fO () else fS (n-1))".

Extract Constant orb => "(||)".
Extract Constant andb => "(&&)".
Extract Constant negb => "not".
Extraction Inline orb negb andb.

Extract Inlined Constant Datatypes.length => "length".
Extract Inlined Constant Coq.Lists.List.filter => "filter".
Extract Inlined Constant app => "(++)".
Extract Inlined Constant List.map => "map".

Extract Constant option_rect => "flip maybe".
Extraction Inline option_rect option_rec.

Extract Constant fst => "fst".
Extract Constant snd => "snd".
Extraction Inline fst snd.

Extraction "Expr.hs" unfold inlineExpr modifyValues eval.