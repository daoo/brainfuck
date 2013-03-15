Require Import Aux Expr.
Require Import Basics ssreflect ssrint ssrnat ssralg eqtype.
Import intZmod intRing.

Extraction Language Haskell.

Extract Inlined Constant compose => "(.)".

Extract Inductive unit => "()" [ "()" ].

Extract Inductive bool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inductive sumbool => "Prelude.Bool" [ "Prelude.True" "Prelude.False" ].
Extract Inlined Constant orb => "(Prelude.||)".
Extract Inlined Constant andb => "(Prelude.&&)".
Extract Inlined Constant negb => "Prelude.not".

Extract Inductive nat => "Int" [ "0" "(+1)" ].
Extract Inductive int => "Int" [ "id" "negate" ].

Extract Inlined Constant eq_op => "(Prelude.==)".
Extract Inlined Constant eqn => "(Prelude.==)".

Extract Inlined Constant pred => "(Prelude.subtract 1)".
Extract Inlined Constant plus => "(Prelude.+)".
Extract Inlined Constant mult => "(Prelude.*)".

Extract Inlined Constant addz => "(Prelude.+)".
Extract Inlined Constant mulz => "(Prelude.*)".
Extract Inlined Constant oppz => "(Prelude.negate)".

Extract Inductive option => "Prelude.Maybe" [ "Prelude.Just" "Prelude.Nothing" ].
Extract Constant option_rect => "Prelude.flip Prelude.maybe".
Extraction Inline option_rect option_rec.

Extract Inductive prod => "(,)" [ "(,)" ].
Extract Inlined Constant fst => "Prelude.fst".
Extract Inlined Constant snd => "Prelude.snd".

Extract Inductive list => "List" [ "[]" "(:)" ].

Extraction "Expr.hs" mkInt mkGet add mul unfold inlineExpr modifyValues eval.
