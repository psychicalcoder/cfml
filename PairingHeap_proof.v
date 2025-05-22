Set Implicit Arguments.
From CFML Require Import WPLib.
From CFML Require Import Stdlib.
Require Import PairingHeap_ml.
From TLC Require Import LibListZ LibMultiset.
Open Scope Z_scope.


Notation t := Z.

Inductive MTree :=
| Leaf
| Node (val : t) (lt : MTree) (rt : MTree).

Fixpoint size (tree : MTree) : nat :=
  match tree with
  | Leaf => 0
  | Node val lt rt => 1 + size lt + size rt
  end.

Fixpoint length (tree : MTree) : nat :=
  match tree with
  | Leaf => 0
  | Node _ _ rt => 1 + length rt
  end.

Fixpoint Φ (tree : MTree) : Z:=
  match tree with
  | Leaf => 0
  | Node val lt rt => Z.log2_up (size (Node val lt rt)) + Φ lt + Φ rt
  end.

Definition MTree_is_root (tree : MTree) :=
  match tree with
  | Leaf => True
  | Node val lt rt => rt = Leaf
  end.

Fixpoint MTree_forall (tree: MTree) (p : t -> Prop) : Prop :=
  match tree with
  | Leaf => True
  | Node val lt rt => p val /\ MTree_forall lt p /\ MTree_forall rt p
  end.

Lemma MTree_Forall_Leaf :
  forall f, MTree_forall Leaf f.
Proof.
  intros. simpl. tauto.
Qed.

Hint Resolve MTree_Forall_Leaf.

(*
Fixpoint MHeap (tr:MTree) (p:loc) : hprop :=
  match tr with
  | Leaf => \[]
  | Node x lt rt =>
      \exists (p1 p2 p3:loc),
          (p ~~~> `{ value' := x; child' := p1; sibling' := p2; parent' := p3 })
    \* (MHeap lt p1)
    \* (MHeap rt p2)
    \* \[ MTree_forall lt (fun x' => x <= x') ]
  end.
*)
                    
Inductive Heap_Ordered : MTree -> Prop :=
| Heap_Ordered_leaf: Heap_Ordered Leaf
| Heap_Ordered_node:
    forall val lt rt,
      Heap_Ordered lt
      -> Heap_Ordered rt
      -> MTree_forall lt (fun x => val <= x)
      -> Heap_Ordered (Node val lt rt).

Hint Resolve Heap_Ordered_leaf Heap_Ordered_node.

(**
type node = {
  mutable value : int;
  child : contents ref;
  sibling : contents ref;
  parent : contents ref
} and contents = Empty | Nonempty of node

type heap = contents ref
 **)

Fixpoint Tree (tr:MTree) (p:loc) { struct tr } : hprop :=
  match tr with
  | Leaf => p ~~> Empty
  | Node x lt rt =>
      \exists (qlt qrt qpar:loc),
          p ~~~> `{ value':= x; child' := qlt; sibling' := qrt; parent' := qpar }
            \* qlt ~> Tree lt \* qrt ~> Tree rt
  end.

Definition MHeap (tr:MTree) (p:loc) : hprop :=
  (p ~> Tree tr) \* \[Heap_Ordered tr].


Definition Contents (tr:MTree) (c:contents_) : hprop :=
  match c with
  | Empty => \[ tr = Leaf ]
  | Nonempty p => p ~> MHeap tr \* \[ tr <> Leaf ]
  end.


Definition Heap (tr:MTree) (p:heap_) : hprop :=
  \exists c, p ~~> c \* Contents tr c.

Lemma Contents_isEmpty : forall (tr:MTree) (c:contents_),
    Contents tr c ==> \[ tr = Leaf <-> c = Empty ] \* (Contents tr c).
Proof.
  intros.
  unfolds Contents.
  destruct c; xsimpl*.
  split; congruence.
Qed.

Lemma Triple_create :
  SPEC (create tt)
    PRE \[]
    POST (fun p => p ~> Heap Leaf).
Proof.
  xcf. xapp. xunfold Heap. unfold Contents. xsimpl*.
Qed.

Lemma Triple_isEmpty : forall (p: loc) (tr: MTree),
  SPEC (is_empty p)
    PRE (p ~> Heap tr)
    POST (fun b => \[b = isTrue(tr = Leaf)] \* (p ~> Heap tr)).
Proof.
  xcf.
  xunfolds Heap; => q.
  xapp. xapp.
  xchanges~ Contents_isEmpty.
  intros H.
  symmetry.
  exact H.
Qed.

Fixpoint MTree_to_MultiSet (tr:MTree) : multiset t :=
  match tr with
  | Leaf => \{}
  | Node x lt rt => \{ x } \u (MTree_to_MultiSet lt) \u (MTree_to_MultiSet rt)
  end.

(*
let merge_nodes q1 q2 =
  if q1.value < q2.value
    then (q2.parent := (match !(q1.child) with
                        | Empty -> Nonempty q1
                        | Nonempty _ -> Empty);
q2.sibling := !(q1.child) ;
q1.child := Nonempty q2 ;
q1)

    else (q1.parent := (match !(q2.child) with | Empty -> Nonempty q2 | Nonempty _ -> Empty);
q1.sibling := !(q2.child) ;
q2.child := Nonempty q1 ; q2)
 *)

Require Import Coq.ZArith.BinInt.

Definition MTree_merge_node (t1 t2: MTree) : MTree :=
  match (t1, t2) with
  | (Leaf, _) => Leaf
  | (_, Leaf) => Leaf
  | (Node x1 lt1 rt1, Node x2 lt2 rt2) => 
      if x1 <? x2 then
        Node x1 (Node x2 lt2 lt1) Leaf
      else
        Node x2 (Node x1 lt1 lt2) Leaf
  end.

Lemma MTree_forall_if : forall tr f1 f2,
    (forall x, f1 x -> f2 x) ->
    MTree_forall tr f1 ->
    MTree_forall tr f2.
Proof.
  intros.
  induction tr.
  - auto.
  - simpl; simpl in H0; destruct H0 as [H01 [H02 H03]].
    repeat split; auto.
Qed.

Lemma MTree_merge_node_heap_ordered : forall (tr1 tr2 tret: MTree),
    tr1 <> Leaf -> tr2 <> Leaf ->
    MTree_is_root tr1 -> MTree_is_root tr2 ->
    Heap_Ordered tr1 -> Heap_Ordered tr2 ->
    tret = (MTree_merge_node tr1 tr2) ->
    Heap_Ordered tret.
Proof.
  intros.
  subst.
  unfold MTree_merge_node.
  destruct tr1; destruct tr2; simpl in *.
  - congruence.
  - congruence.
  - assumption.
  - destruct (val <? val0) eqn:E.
    + apply Z.ltb_lt in E.
      inversion H3; subst.
      inversion H4; subst.
      apply Heap_Ordered_node; auto.
      * simpl. repeat split.
        -- apply Z.lt_le_incl; exact E.
        -- apply MTree_forall_if with (f1 := (fun (x:t) => val0 <= x)) (f2 := (fun (x:t) => val <= x)).
           { intros. lia. }
           assumption.
        -- assumption.
    + apply Z.ltb_ge in E.
      inversion H3; subst.
      inversion H4; subst.
      apply Heap_Ordered_node; auto.
      * simpl. repeat split; try assumption.
        -- apply MTree_forall_if with (f1 := (fun (x:t) => val <= x)) (f2 := (fun (x:t) => val0 <= x)).
           { intros. lia. }
           assumption.
Qed.

Lemma MTree_merge_node_union : forall (tr1 tr2 tret: MTree),
    tr1 <> Leaf -> tr2 <> Leaf ->
    MTree_is_root tr1 -> MTree_is_root tr2 ->
    Heap_Ordered tr1 -> Heap_Ordered tr2 ->
    MTree_to_MultiSet (MTree_merge_node tr1 tr2) = (MTree_to_MultiSet tr1) \u (MTree_to_MultiSet tr2).
Proof.
  intros.
  unfold MTree_merge_node.
  destruct tr1; destruct tr2; simpl in *.
  - auto.
  - congruence.
  - congruence.
  - subst. destruct (val <? val0) eqn:E.
    all : simpl; repeat rewrite for_multiset_union_empty_r; permut_simpl.      
Qed.

(*
let insert p x =
  let rec q2 = { value = x; child = ref Empty; sibling = ref Empty; parent = ref Empty } in
  match !p with
  | Empty -> p := Nonempty q2
  | Nonempty q1 -> if is_empty q1.child then q2.parent := !p; p := Nonempty (merge_nodes q1 q2)
 *)

Definition MTree_insert (tr: MTree) (x: t) : MTree :=
  match tr with
  | Leaf => Node x Leaf Leaf
  | Node x' lt rt =>
      MTree_merge_node tr (Node x Leaf Leaf)
  end.

Lemma MTree_insert_heap_ordered : forall tr x,
    Heap_Ordered tr ->
    MTree_is_root tr ->
    Heap_Ordered (MTree_insert tr x).
Proof.
  intros.
  unfold MTree_insert.
  destruct tr; simpl in *.
  - auto.
  - subst. apply MTree_merge_node_heap_ordered with (tr1 := (Node val tr1 Leaf)) (tr2 := (Node x Leaf Leaf)).
    + discriminate.
    + discriminate.
    + simpl. reflexivity.
    + simpl. reflexivity.
    + assumption.
    + apply Heap_Ordered_node.
      * apply Heap_Ordered_leaf.
      * apply Heap_Ordered_leaf.
      * simpl. tauto.
    + reflexivity.
Qed.

Lemma MTree_node_multiset : forall x m1 m2,
    MTree_to_MultiSet (Node x m1 m2) = \{x} \u MTree_to_MultiSet m1 \u MTree_to_MultiSet m2.
Proof.
  intros.
  simpl.
  permut_simpl.
Qed.

Lemma MTree_single_multiset : forall x,
    MTree_to_MultiSet (Node x Leaf Leaf) = \{x}.
Proof.
  intros. simpl. permut_simpl.
Qed.

Lemma MTree_insert_multiset_union_single : forall tr x,
    Heap_Ordered tr -> MTree_is_root tr ->
    MTree_to_MultiSet (MTree_insert tr x) = MTree_to_MultiSet tr \u \{ x }.
Proof.
  intros.
  unfold MTree_insert.
  destruct tr eqn: E; simpl.
  - repeat rewrite for_multiset_union_empty_l.
    repeat rewrite for_multiset_union_empty_r.
    reflexivity.
  - rewrite <- MTree_node_multiset.
    rewrite <- MTree_single_multiset.
    apply MTree_merge_node_union; auto.
    * discriminate.
    * discriminate.
    * simpl. reflexivity.
Qed.

(*
let rec merge_siblings q =
  match !(q.sibling) with
  | Empty -> q
  | Nonempty q1 -> let q2 = merge_nodes q q1 in
    match !(q1.sibling) with
    | Empty -> q2
    | Nonempty q3 -> merge_nodes q2 (merge_siblings q3)
 *)

Fixpoint MTree_merge_sibilings (x: t) (chld sibl: MTree) : MTree :=
  match sibl with
  | Leaf => Node x chld Leaf
  | Node x' chld' sibl' =>
      let q := MTree_merge_node (Node x chld Leaf) (Node x' chld' Leaf) in
      match sibl' with
      | Leaf => q
      | Node x'' chld'' sibl'' =>
          MTree_merge_node q
            (MTree_merge_sibilings x'' chld'' sibl'')
      end
  end.

Lemma MTree_merge_sibilings_heap_ordered :
  forall x chld sibl,
    Heap_Ordered (Node x chld sibl) ->
    Heap_Ordered (MTree_merge_sibilings x chld sibl).
Proof.
  intros.
  inversion H; subst.  
  induction sibl; simpl.
  - apply Heap_Ordered_node; assumption.
  - destruct sibl2.
    + apply MTree_merge_node_heap_ordered with (tr1 := (Node x chld Leaf)) (tr2 := (Node val sibl1 Leaf)); simpl; auto.
      all: discriminate.
    + apply MTree_merge_node_heap_ordered with (tr1 := (MTree_merge_node (Node x chld Leaf) (Node val sibl1 Leaf))) (tr2 := (MTree_merge_sibilings val0 sibl2_1 sibl2_2)); simpl; auto.
      * unfold MTree_merge_node.
        destruct (x <? val); discriminate.
Admitted.

Lemma MTree_merge_sibilings_union :
  forall x chld sibl,
    MTree_to_MultiSet (MTree_merge_sibilings x chld sibl) =
      \{x} \u MTree_to_MultiSet chld \u MTree_to_MultiSet sibl.
Proof.
Admitted.

(*
let pop_min p =
  match !p with
  | Empty -> assert false
  | Nonempty q ->
    let x = q.value in
    (match !(q.child) with
    | Empty -> p := Empty
    | Nonempty child -> p := Nonempty (merge_siblings child));
    x
 *)

Definition MTree_pop_min (tr: MTree) : option (t * MTree) :=
  match tr with
  | Leaf => None
  | Node x chld sibl =>
      match chld with
      | Leaf => Some (x, Leaf)
      | Node x' chld' sibl' =>
          Some (x, MTree_merge_sibilings x' chld' sibl')
      end
  end.

Lemma MTree_pop_min_minimal :
  forall (ret: t) (tr tr': MTree),
    Heap_Ordered tr -> 
    MTree_is_root tr ->
    MTree_pop_min tr = Some (ret, tr') ->
    MTree_forall tr (fun (x:t) => ret <= x).
Proof.
  intros.
  destruct tr; simpl in *.
  - congruence.
  - subst.
    assert (E: ret = val).
    { destruct tr1; inversion H1; subst; auto. }
    subst.
    repeat split.
    reflexivity.
    all: inversion H; subst; auto.
Qed.

Lemma MTree_pop_min_heap :
  forall (ret: t) (tr tr': MTree),
    Heap_Ordered tr ->
    MTree_is_root tr ->
    MTree_pop_min tr = Some (ret, tr') ->
    Heap_Ordered tr' /\ MTree_is_root tr'.
Proof.
  intros.
  destruct tr; simpl in *.
  - congruence.
  - subst.
    destruct tr1.
    + inversion H1; subst.
      simpl.
      split.
      apply Heap_Ordered_leaf.
      tauto.
    + inversion H1; subst.
      inversion H; subst.
      split.
      * apply MTree_merge_sibilings_heap_ordered.
        assumption.
      * {
          induction tr1_2.
          - unfold MTree_merge_sibilings. simpl. reflexivity.
          - simpl.
            destruct tr1_2_2.
            + unfold MTree_merge_node; destruct (val0 <? val); simpl; reflexivity.
            + unfold MTree_merge_node; destruct (val0 <? val); simpl; destruct (MTree_merge_sibilings val1 tr1_2_2_1 tr1_2_2_2); simpl; try tauto.
              destruct (val0 <? val2); simpl; reflexivity.
              destruct (val <? val2); simpl; reflexivity.
        }
Qed.

Lemma MTree_pop_min_union :
  forall (ret: t) (tr tr': MTree),
    MTree_is_root tr ->
    MTree_pop_min tr = Some (ret, tr') ->
    MTree_to_MultiSet tr  = MTree_to_MultiSet tr' \u \{ret}.
Proof.
  intros.
  destruct tr; simpl in *.
  - congruence.
  - subst.
    destruct tr1.
    + inversion H0; subst.
      permut_simpl.
    + inversion H0; subst.
      rewrite MTree_merge_sibilings_union.
      simpl.
      permut_simpl.
Qed.
  
Lemma Tree_Leaf : forall p,
    (p ~> Tree Leaf) = p ~~> Empty.
Proof.
  auto.
Qed.

Lemma Tree_Node : forall p x lt rt,
    (p ~> Tree (Node x lt rt)) =
      \exists (p1 p2 p3:loc),
          p ~~~> `{ value' := x; child' := p1; sibling' := p2; parent' := p3}
            \* (p1 ~> Tree lt) \* (p2 ~> Tree rt).
Proof.
  auto.
Qed.

Lemma Heap_Nonempty : forall p q tr,
  p ~~> Nonempty q \* q ~> MHeap tr ==> p ~> Heap tr.
Proof using.
  intros. xunfold Heap. xunfold Contents. xsimpl.
Qed.

Lemma Triple_merge_nodes : forall (q1 q2: loc) (tr1 tr2: MTree) (x1 x2: t) (lt1 rt1 lt2 rt2: MTree),
  (tr1 = Node x1 lt1 rt1) -> (tr2 = Node x2 lt2 rt2) ->
  MTree_is_root tr1 -> MTree_is_root tr2 ->                            
  x1 < x2 ->
  SPEC (merge_nodes q1 q2)
    PRE (q1 ~> MHeap tr1) \* (q2 ~> MHeap tr2)
    POST (fun qret => qret ~> Heap (MTree_merge_node tr1 tr2)).
Proof.
  intros.
  simpl in *.
  subst.
  xcf.
  xunfold MHeap.
  xunfold Tree.
  xpull.
  intros p1 p2 p3 HT1 p4 p5 p6 HT2.
  xsimpl*.
  xif; => C.
  xapp.

  destruct lt1.
  - (* q1.child is Empty *)
    xchange Tree_Leaf.
    xapp.
    xlet.
    xcase.
    + xval.
    + xsimpl*.
      intro; subst.
      xapp.
      
      
      
Admitted.
(**

Formalization of pairing heaps, covering both
- purely functional pairing heaps (in Coq code)
- ephemeral (pointer-based) pairing heaps in CFML2

More information about pairing heaps:
  https://www.cise.ufl.edu/~sahni/dsaaj/enrich/c13/pairing.htm

Author: Arthur Charguéraud.
License: CC-by 4.0.

*)

(* ********************************************************************** *)
(* ********************************************************************** *)
(* ********************************************************************** *)
(** * General Definitions *)


(* ********************************************************************** *)
(** ** Types of elements *)

(** For simplicity, assume the priority queue to store integer values.
    It is not hard to generalize everything to any ordered type. *)

Notation "'elem'" := (int).
Notation "'elems'" := (multiset elem).

(* ********************************************************************** *)
(** ** List unions *)

(** [list_union Es] computes the iterated union of the multisets in the list [Es] *)

Definition list_union (Es:list elems) : elems :=
  LibList.fold_right union \{} Es.


(** Normalization lemmas for [list_union] *)

Lemma list_union_nil :
  list_union (@nil elems) = \{}.
Proof using. auto. Qed.

Lemma list_union_cons : forall E Es,
  list_union (E::Es) = E \u list_union Es.
Proof using. auto. Qed.

(** Hints *)

Hint Rewrite list_union_nil list_union_cons : rew_listx.
Hint Rewrite (@union_empty_r elems _ _ _) (@union_empty_l elems _ _ _) : rew_listx.

Hint Constructors Forall Forall2 list_sub.


(* ********************************************************************** *)
(** ** Minimal elements *)

(** Auxiliary definition for specifications *)

Definition min_of (E:elems) (x:elem) : Prop :=
  x \in E /\ forall_ y \in E, x <= y.

(** Auxiliary definition for stating invariants follow. *)

(** [is_ge x] is a predicate that characterizes items no less than [x] *)

Definition is_ge (x y:elem) : Prop :=
  x <= y.

(** Hints *)

Hint Unfold is_ge.
Hint Extern 1 (_ < _) => simpl; math.
Hint Extern 1 (_ <= _) => simpl; math.
Hint Extern 1 (_ = _ :> multiset _) => rew_listx; multiset_eq.
Hint Extern 1 (_ \in _) => multiset_in.

(** Lemmas to manipulate the invariant [Forall (foreach (is_ge x)) Es] *)

Lemma Forall_foreach_is_ge_inv : forall x y Es,
  Forall (foreach (is_ge x)) Es ->
  y \in list_union Es ->
  x <= y.
Proof using.
  introv M Hy. unfolds list_union. induction M; rew_listx in *.
  { multiset_in Hy. }
  { multiset_in Hy. { applys* H. } { applys* IHM. } }
Qed.

Lemma foreach_list_union : forall P Es,
  Forall (foreach P) Es ->
  foreach P (list_union Es).
Proof using.
  introv M. induction M.
  { applys foreach_empty. }
  { unfold list_union; rew_listx. applys* foreach_union. }
Qed.

Lemma pop_min_lemma : forall x Es,
  Forall (foreach (is_ge x)) Es ->
  min_of (\{x} \u list_union Es) x.
Proof.
  introv M. split.
  { auto. }
  { intros y Hy. multiset_in Hy.
    { auto. } { applys* Forall_foreach_is_ge_inv Es. } }
Qed.


(* ********************************************************************** *)
(* ********************************************************************** *)
(* ********************************************************************** *)
(** * Representation and lemmas *)


(* ******************************************************* *)
(** ** Data structure and definitions *)

(** Functional representation of a node in a (nonempty) pairing heap *)

Inductive node : Type :=
  | Node : elem -> list node -> node.

Instance Inhab_node : Inhab node.
Proof using. applys Inhab_of_val (Node arbitrary nil). Qed.

(** Functional representation of a possibly-empty pairing heap *)

Definition heap := option node.

Instance Inhab_heap : Inhab heap.
Proof using. applys Inhab_of_val (@None node). Qed.

(** [inv n E] relates a tree node [n] with the multiset [E] made of
    the items that the tree contains *)

Inductive inv : node -> elems -> Prop :=
  | inv_Node : forall x ns Es E,
      Forall2 inv ns Es ->
      Forall (foreach (is_ge x)) Es ->
      E = \{x} \u (list_union Es) ->
      inv (Node x ns) E.


(* ******************************************************* *)
(** ** Lemmas and tactics *)

(** An induction principle for trees -- should be automatically generated *)

Section Node_induct.
Variables
(P : node -> Prop)
(Q : list node -> Prop)
(P2 : forall x l, Q l -> P (Node x l))
(Q1 : Q nil)
(Q2 : forall t l, P t -> Q l -> Q (t::l)).

Fixpoint node_induct_gen (n : node) : P n :=
  match n as x return P x with
  | Node x l => P2 x
      ((fix node_list_induct (l : list node) : Q l :=
      match l as x return Q x with
      | nil   => Q1
      | t::l' => Q2 (node_induct_gen t) (node_list_induct l')
      end) l)
  end.

End Node_induct.

Lemma node_induct : forall (P : node -> Prop),
  (forall (x : int) (l : list node),
    (forall n, mem n l -> P n) -> P (Node x l)) ->
  forall n : node, P n.
Proof using.
  introv Hn. eapply node_induct_gen with (Q := fun l =>
    forall t, mem t l -> P t); intros.
  auto. auto. inversions H. inversions~ H1.
Qed.

(** Implicit Types *)

Implicit Types n : node.
Implicit Types p q l : loc.
Implicit Types x y : elem.
Implicit Types h : heap.
Implicit Types hs : list node.
Implicit Types E : elems.
Implicit Types Es : list elems.

(** Key auxiliary lemmas for the verification proofs
    (both for the functional version and the imperative version) *)

Lemma inv_not_empty : forall n E,
  inv n E ->
  E <> \{}.
Proof using. introv I. inverts I. multiset_inv. Qed.

Lemma merge_lemma : forall x1 x2 ns1 ns2 Es1 Es2,
  Forall2 inv ns1 Es1 ->
  Forall2 inv ns2 Es2 ->
  Forall (foreach (is_ge x2)) Es1 ->
  Forall (foreach (is_ge x1)) Es2 ->
  x1 <= x2 ->
  inv (Node x1 (Node x2 ns1 :: ns2)) ('{x1} \u '{x2} \u list_union Es1 \u list_union Es2).
Proof using.
  introv Is1 Is2 Ks1 Ks2 L. applys_eq inv_Node. constructor.
  { applys* inv_Node. }
  { eauto. }
  { constructors.
    { applys foreach_union.
      { applys* foreach_single. }
      { applys* foreach_list_union. applys Forall_pred_incl Ks1.
        { intros x Hx. applys* foreach_weaken. { intros y Hy. unfolds* is_ge. } } } }
    { eauto. } }
  { autos*. }
Qed.



(* ********************************************************************** *)
(* ********************************************************************** *)
(* ********************************************************************** *)
(** * Imperative pairing heaps *)

(* ******************************************************* *)
(** ** Representation predicates *)

(** [q ~> Tree n] is a notation for [Tree n q]. It relates a pointer [q] with the
    functional tree structure [n] that it represents in memory *)

Fixpoint Tree (n:node) (q:loc) { struct n } : hprop :=
  match n with
  | Node x hs =>
      \exists (q':loc),
         q  ~~~>`{ value' := x; child' := q' }
      \* q' ~> MListOf Tree hs
  end.

(** [q ~> Repr E] related a pointer [q] with the multiset of items [E]
    that are stored in the tree *)

Definition Repr (E:elems) (q:loc) : hprop :=
  \exists n, q ~> Tree n \* \[inv n E].

(** [q ~> Heap E] relates a pointer on a heap [p] with the multiset of items [E]
    that are stored in the heap. It uses [Contents E c] as an auxiliary definition. *)

Definition Contents (E:elems) (c:contents_) : hprop :=
  match c with
  | Empty => \[E = \{}]
  | Nonempty p => (p ~> Repr E)
  end.

Definition Heap (E:elems) (p:heap_) : hprop :=
  \exists c, p ~~> c \* Contents E c.


(* ******************************************************* *)
(** ** Paraphrase definitions as equalities *)

Lemma Tree_Node : forall q x hs,
  q ~> Tree (Node x hs) =
      \exists l, q ~~~> `{ value' := x; child' := l }
              \* l ~> MListOf Tree hs.
Proof using. auto. Qed.

Lemma Contents_eq : forall E c,
  Contents E c = (match c with
  | Empty => \[E = \{}]
  | Nonempty p => (p ~> Repr E)
  end).
Proof using. auto. Qed.

Lemma Heap_eq : forall p E,
  p ~> Heap E = \exists c, p ~~> c \* Contents E c.
Proof using. auto. Qed.

Lemma Repr_eq : forall q E,
  q ~> Repr E = \exists n, q ~> Tree n \* \[inv n E].
Proof using. auto. Qed.

Lemma haffine_Tree : forall n p,
  haffine (p ~> Tree n).
Proof using.
  intros n. induction n using node_induct.
  intros. xunfold Tree. xaffine.
Qed.

Hint Resolve haffine_Tree : haffine.


(* ******************************************************* *)
(** ** Lemmas about representation predicates *)

Lemma Repr_not_empty : forall q E,
  q ~> Repr E ==> \[E <> \{}] \* q ~> Repr E.
Proof using.
  intros. xunfold Repr. xpull ;=> n I. lets: inv_not_empty I. xsimpl*.
Qed.

Lemma Contents_is_empty : forall c E,
  Contents E c ==> \[c = Empty <-> E = \{}] \* Contents E c.
Proof using.
  intros.  unfold Contents. destruct c.
  { xsimpl*. }
  { xchange Repr_not_empty ;=> N. xsimpl. iff H; false. }
Qed.

Lemma Heap_Nonempty : forall p q E,
  p ~~> Nonempty q \* q ~> Repr E ==> p ~> Heap E.
Proof using.
  intros. xchanges Repr_not_empty ;=> N. xunfold Heap. xsimpl.
Qed.

Lemma Heap_Empty : forall p,
  p ~~> Empty ==> p ~> Heap \{}.
Proof using. intros. xunfold Heap. unfold Contents. xsimpl*. Qed.


(* ******************************************************* *)
(** ** Verification *)

Lemma Triple_create :
  SPEC (create tt)
    PRE \[]
    POST (fun p => p ~> Heap \{}).
Proof using.
  xcf. xapp. xunfold Heap. unfold Contents. xsimpl*.
Qed.

Hint Extern 1 (RegisterSpec create) => Provide Triple_create.

Lemma Triple_is_empty : forall p E,
  SPEC (is_empty p)
    PRE (p ~> Heap E)
    POST (fun b => \[b = isTrue (E = \{})] \* p ~> Heap E).
Proof using.
  xcf. xunfolds Heap ;=> q. xapp. xapp.
  xchanges~ Contents_is_empty.
Qed.

Hint Extern 1 (RegisterSpec (is_empty)) => Provide Triple_is_empty.

Lemma Triple_merge : forall q1 q2 E1 E2,
  SPEC (merge q1 q2)
    PRE (q1 ~> Repr E1 \* q2 ~> Repr E2)
    POST (fun q => q ~> Repr (E1 \u E2)).
Proof using.
  xcf. xchange (Repr_eq q1) ;=> [x1 hs1] I1.
  xchange (Repr_eq q2) ;=> [x2 hs2] I2.
  xchange (Tree_Node q1) ;=> l1.
  xchange (Tree_Node q2) ;=> l2.
  inverts I1 as Is1 Ks1. inverts I2 as Is2 Ks2.
  xapp. 
  xif ;=> C.
  { xapp. xchange <- (Tree_Node q2). xapp.
    xchange <- Tree_Node. xchange <- Repr_eq.
    applys* merge_lemma. xvals*. }
  { xapp. xchange <- (Tree_Node q1). xapp.
    xchange <- Tree_Node. xchange <- Repr_eq.
    applys* merge_lemma. xvals*. }
Qed.

Hint Extern 1 (RegisterSpec merge) => Provide Triple_merge.

Lemma Triple_insert : forall p x E,
  SPEC (insert p x)
    PRE (p ~> Heap E)
    POST (fun (_:unit) => p ~> Heap (E \u \{x})).
Proof using.
  xcf. xchange Heap_eq ;=> q. xapp ;=> l. xapp ;=> q2.
  xchange <- Tree_Node. xchange <- Repr_eq. { applys* inv_Node. }
  rew_listx. xapp. xmatch; simpl.
  { xpull ;=> ->. xapp. xchanges* Heap_Nonempty. }
  { xapp ;=> r. xapp. xchanges* Heap_Nonempty. }
Qed.

Hint Extern 1 (RegisterSpec insert) => Provide Triple_insert.

Lemma Triple_merge_pairs : forall ns l Es,
  ns <> nil ->
  Forall2 inv ns Es ->
  SPEC (merge_pairs l)
    PRE (l ~> MListOf Tree ns)
    POST (fun q => q ~> Repr (list_union Es)).
Proof using.
  intros ns. induction_wf IH: list_sub ns; introv N Is.
  xcf. xapp~ ;=> q1 n1 ns' ->. inverts Is as I1 Is. rename r into Es'.
  xif ;=> C.
  { subst. inverts Is. rew_listx. xval. xchanges* <- Repr_eq. }
  { xapp~ ;=> q2 n2 ns'' ->. inverts Is as I2 Is. rename r into Es''.
    do 2 xchange* <- Repr_eq. xapp ;=> r. xif ;=> C'.
    { subst. inverts Is. rew_listx. xval. xsimpl. }
    { xapp* ;=> r'. xapp ;=> r''. rew_listx. xsimpl*. } }
Qed.

Hint Extern 1 (RegisterSpec merge_pairs) => Provide Triple_merge_pairs.

Lemma Triple_pop_min : forall p E,
  E <> \{} ->
  SPEC (pop_min p)
    PRE (p ~> Heap E)
    POST (fun x => \exists E', \[min_of E x /\ E = \{x} \u E'] \* p ~> Heap E').
Proof using.
  introv HE. xcf. xchange Heap_eq ;=> c. xapp.
  destruct c as [|q]; simpl; xpull.
  xchange Repr_eq ;=> [x hs] I. invert I ;=> ? ? ? ? Is Ks Eq -> -> ->.
  xchange Tree_Node ;=> l. xmatch. xapp. xapp. xapp.
  xseq (fun (_:unit) => \exists E', \[E = '{x} \u E'] \* p ~> Heap E' \* \GC).
  { xif ;=> C2.
    { subst. inverts Is. inverts Ks. rew_listx. xapp. xchanges* Heap_Empty. }
    { xapp. xapp* ;=> r. xapp. xchange Heap_Nonempty. xsimpl*. } }
  { xpull ;=> E' ->. xval. xsimpl. split~. { rewrite Eq. applys~ pop_min_lemma. } }
Qed.

Hint Extern 1 (RegisterSpec pop_min) => Provide Triple_pop_min.
