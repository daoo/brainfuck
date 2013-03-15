Require Import ssreflect ssrnat ssrint ssrbool eqtype.

Set Implicit Arguments.
Unset Strict Implicit.
Unset Printing Implicit Defensive.

Section Aux_defs.
  Fixpoint eqn (n m: nat) : bool :=
    match n, m with
    | O, O       => true
    | O, S _     => false
    | S _, O     => false
    | S n', S m' => eqn n' m'
    end.

  Definition eqz (a b: int) : bool :=
    match a, b with
    | Posz a', Posz b' => eqn a' b'
    | Negz a', Negz b' => eqn a' b'
    | _, _             => false
    end.
End Aux_defs.
