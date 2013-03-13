Require Import Expr ssreflect ssrint seq.
Extraction Language Haskell.

Extract Inductive unit => "()" [ "()" ].

Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive sumbool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Constant orb => "(Prelude.||)".
Extract Constant andb => "(Prelude.&&)".
Extract Constant negb => "Prelude.not".
Extraction Inline orb negb andb.

Extract Inductive option => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Constant option_rect => "Prelude.flip Prelude.maybe".
Extraction Inline option_rect option_rec.

Extract Inductive prod => "(,)" [ "(,)" ].
Extract Constant fst => "Prelude.fst".
Extract Constant snd => "Prelude.snd".
Extraction Inline fst snd.

Extraction "Expr.hs" mkInt mkGet add mul unfold inlineExpr modifyValues eval.
