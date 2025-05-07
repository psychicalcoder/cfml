Set Implicit Arguments.
From CFML Require Import WPLib.
From CFML Require Import Stdlib.
From EXAMPLES Require Import Credits2_ml.
From TLC Require Import LibOrder LibInt.
Generalizable Variables A.

Implicit Types n m : int.
Implicit Types p q : loc.


Lemma incr_spec : forall (p:loc) (n:int),
  SPEC (incr p)
    PRE ((\$ 1) \* (p ~~> n))
    POSTUNIT (p ~~> (n+1)).
Proof using.
  xcf_pay. xapp. xapp. xsimpl. 
Qed.

Hint Extern 1 (RegisterSpec incr) => Provide incr_spec.

Lemma sum_spec : forall p n m,
    m >= 0 ->
  SPEC (sum p m)
    PRE ((\$ (2*m + 1)) \* (p ~~> n))
    POSTUNIT (p ~~> (n + m)).
Proof using.
  introv Hm. gen n Hm. induction_wf IH: (downto 0) m. intros.
  xcf_pay. xif; intro C.
  + xapp. xapp. auto with maths. auto with maths.
    xsimpl. auto with maths.
  + xvals. auto with maths.
Qed.
    
  

(* Lemma Triple_sum : forall n, *)
(*   SPEC (sum n) *)
(*     PRE ( \$ (n+1) ) *)
(*     POST (fun r => (\$ 0) \* \[r = n * (n + 1) / 2]). *)
(* Proof using. *)
(*   intro n. *)
(*   induction_wf IH: (downto 0) n. *)
(*   xcf_pay. *)
(*   xsimpl. *)
(*   xif; intros C. *)
(*   { xval. *)
(*     xsimpl. *)
(*     subst. *)
(*     inversion C. *)
(*     rewrite isTrue_eq_true_eq in H0. *)
(*     subst. *)
(*     rewrite mult_zero_l. *)
(*     rewrite Zdiv_0_l. *)
(*     reflexivity. *)
(*     subst. *)
(*     inversion C. *)
(*     rewrite isTrue_eq_true_eq in H0. *)
(*     subst. *)
(*     auto with maths. *)
(*   } *)
(*   Admitted. *)
