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

  Fixpoint gtn (n m : nat) : bool :=
    match n, m with
    | O, O   => false
    | O, S _ => false
    | S _, O => true
    | S n', S m' => gtn n' m'
    end.

  Definition gtz (a b : int) : bool :=
    match a, b with
    | Posz a', Posz b' => gtn a' b'
    | Negz a', Negz b' => gtn b' a'
    | Posz _, Negz _   => true
    | Negz _, Posz _   => false
    end.
End Aux_defs.
