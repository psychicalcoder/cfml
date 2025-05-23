Set Implicit Arguments.
From CFML Require Import WPLib.
From CFML Require Import Stdlib.
Require Import PairingHeap_ml.
From TLC Require Import LibMultiset.
Open Scope Z_scope.


Notation t := Z.

Inductive Tree :=
| Leaf
| Node (val : t) (lt : Tree) (rt : Tree).

Fixpoint size (tree : Tree) : Z :=
  match tree with
  | Leaf => 0
  | Node val lt rt => 1 + size lt + size rt
  end.

Fixpoint length (tree : Tree) : Z :=
  match tree with
  | Leaf => 0
  | Node _ _ rt => 1 + length rt
  end.

Fixpoint Φ (tree : Tree) : Z:=
  match tree with
  | Leaf => 0
  | Node val lt rt => Z.log2_up (size (Node val lt rt)) + Φ lt + Φ rt
  end.

Fixpoint Tree_Forall (tree: Tree) (p : t -> Prop) : Prop :=
  match tree with
  | Leaf => True
  | Node val lt rt => p val /\ Tree_Forall lt p /\ Tree_Forall rt p
  end.

Lemma Tree_Forall_Leaf :
  forall f, Tree_Forall Leaf f.
Proof.
  intros. simpl. tauto.
Qed.

Lemma Tree_Forall_imply : forall tr f1 f2,
    (forall x, f1 x -> f2 x) ->
    Tree_Forall tr f1 ->
    Tree_Forall tr f2.
Proof.
  intros.
  induction tr.
  - auto.
  - simpl; simpl in H0; destruct H0 as [H01 [H02 H03]].
    repeat split; auto.
Qed.

Hint Resolve Tree_Forall_Leaf Tree_Forall_imply.

Fixpoint Tree_to_MSet (tr:Tree) : multiset t :=
  match tr with
  | Leaf => \{}
  | Node x lt rt => \{ x } \u (Tree_to_MSet lt) \u (Tree_to_MSet rt)
  end.
                    
Inductive Heap_Ordered : Tree -> Prop :=
| Heap_Ordered_leaf: Heap_Ordered Leaf
| Heap_Ordered_node:
    forall val lt rt,
      Heap_Ordered lt
      -> Heap_Ordered rt
      -> Tree_Forall lt (fun x => val <= x)
      -> Heap_Ordered (Node val lt rt).

Hint Resolve Heap_Ordered_leaf Heap_Ordered_node.

(** Pure functional paring heap operations **)

Require Import Coq.ZArith.BinInt.

Definition Tree_is_nonempty_heap (tree : Tree) :=
  match tree with
  | Leaf => False
  | Node val lt rt => rt = Leaf
  end.

Definition Tree_is_heap (tree : Tree) :=
  match tree with
  | Leaf => True
  | Node val lt rt => rt = Leaf
  end.


Definition Tree_merge_node (t1 t2: Tree) : Tree :=
  match (t1, t2) with
  | (Leaf, _) => t2
  | (_, Leaf) => t1
  | (Node x1 lt1 rt1, Node x2 lt2 rt2) => 
      if x1 <? x2 then
        Node x1 (Node x2 lt2 lt1) Leaf
      else
        Node x2 (Node x1 lt1 lt2) Leaf
  end.

Lemma Tree_merge_node_heap_ordered : forall (tr1 tr2: Tree),
    Tree_is_heap tr1 -> Tree_is_heap tr2 ->
    Heap_Ordered tr1 -> Heap_Ordered tr2 ->
    Heap_Ordered (Tree_merge_node tr1 tr2).
Proof.
  intros.
  destruct tr1; destruct tr2; simpl in *; auto.
  { subst.
    unfold Tree_merge_node.
    destruct (val <? val0) eqn:E.
    - apply Z.ltb_lt in E.
      inversion H1; subst.
      inversion H2; subst.
      apply Heap_Ordered_node; auto.
      + simpl. repeat split.
        * apply Z.lt_le_incl; exact E.
        * apply Tree_Forall_imply with (f1 := (fun (x:t) => val0 <= x)) (f2 := (fun (x:t) => val <= x)).
           { intros. lia. }
           assumption.
        * assumption.
    - apply Z.ltb_ge in E.
      inversion H1; subst.
      inversion H2; subst.
      apply Heap_Ordered_node; auto.
      * simpl. repeat split; try assumption.
        -- apply Tree_Forall_imply with (f1 := (fun (x:t) => val <= x)) (f2 := (fun (x:t) => val0 <= x)).
           { intros. lia. }
           assumption.
  }
Qed.

Lemma Tree_merge_node_union : forall (tr1 tr2 tret: Tree),
    Tree_is_heap tr1 -> Tree_is_heap tr2 ->
    Heap_Ordered tr1 -> Heap_Ordered tr2 ->
    Tree_to_MSet (Tree_merge_node tr1 tr2) = (Tree_to_MSet tr1) \u (Tree_to_MSet tr2).
Proof.
  intros.
  unfold Tree_merge_node.
  destruct tr1; destruct tr2; simpl in *; try permut_simpl.
  { subst. destruct (val <? val0) eqn:E.
    all : simpl; repeat rewrite for_multiset_union_empty_r; permut_simpl.
  }
Qed.

Definition Tree_insert (tr: Tree) (x: t) : Tree :=
  match tr with
  | Leaf => Node x Leaf Leaf
  | Node x' lt rt =>
      Tree_merge_node tr (Node x Leaf Leaf)
  end.

Lemma Tree_insert_heap_ordered : forall tr x,
    Heap_Ordered tr ->
    Tree_is_heap tr ->
    Heap_Ordered (Tree_insert tr x).
Proof.
  intros.
  unfold Tree_insert.
  destruct tr; simpl in *; subst; auto.
  apply Tree_merge_node_heap_ordered with (tr1 := (Node val tr1 Leaf)) (tr2 := (Node x Leaf Leaf)); simpl in *; auto.
Qed.

Lemma Tree_node_multiset : forall x m1 m2,
    Tree_to_MSet (Node x m1 m2) = \{x} \u Tree_to_MSet m1 \u Tree_to_MSet m2.
Proof.
  intros.
  simpl.
  permut_simpl.
Qed.

Lemma Tree_single_multiset : forall x,
    Tree_to_MSet (Node x Leaf Leaf) = \{x}.
Proof.
  intros. simpl. permut_simpl.
Qed.

Lemma Tree_insert_multiset_union_single : forall tr x,
    Heap_Ordered tr -> Tree_is_heap tr ->
    Tree_to_MSet (Tree_insert tr x) = Tree_to_MSet tr \u \{ x }.
Proof.
  intros.
  unfold Tree_insert.
  destruct tr eqn: E; simpl.
  - permut_simpl.
  - rewrite <- Tree_node_multiset.
    rewrite <- Tree_single_multiset.
    apply Tree_merge_node_union; simpl in *; auto.
Qed.

Hint Resolve Tree_node_multiset Tree_single_multiset Tree_insert_multiset_union_single.

Fixpoint Tree_merge_sibilings (x: t) (chld sibl: Tree) : Tree :=
  match sibl with
  | Leaf => Node x chld Leaf
  | Node x' chld' sibl' =>
      let q := Tree_merge_node (Node x chld Leaf) (Node x' chld' Leaf) in
      match sibl' with
      | Leaf => q
      | Node x'' chld'' sibl'' =>
          Tree_merge_node q
            (Tree_merge_sibilings x'' chld'' sibl'')
      end
  end.

Lemma Tree_merge_sibilings_heap_ordered :
  forall x chld sibl,
    Heap_Ordered (Node x chld sibl) ->
    Heap_Ordered (Tree_merge_sibilings x chld sibl).
Proof.
  intros.
  inversion H; subst.  
  induction sibl; simpl.
  - auto.
  - destruct sibl2.
    + apply Tree_merge_node_heap_ordered with (tr1 := (Node x chld Leaf)) (tr2 := (Node val sibl1 Leaf)); simpl; auto.
    + apply Tree_merge_node_heap_ordered with (tr1 := (Tree_merge_node (Node x chld Leaf) (Node val sibl1 Leaf))) (tr2 := (Tree_merge_sibilings val0 sibl2_1 sibl2_2)); simpl; auto.
      * unfold Tree_merge_node.
        destruct (x <? val); simpl; reflexivity.
Admitted.

Lemma Tree_merge_sibilings_union :
  forall x chld sibl,
    Tree_to_MSet (Tree_merge_sibilings x chld sibl) =
      \{x} \u Tree_to_MSet chld \u Tree_to_MSet sibl.
Proof.
Admitted.

Definition Tree_pop_min (tr: Tree) : option (t * Tree) :=
  match tr with
  | Leaf => None
  | Node x chld sibl =>
      match chld with
      | Leaf => Some (x, Leaf)
      | Node x' chld' sibl' =>
          Some (x, Tree_merge_sibilings x' chld' sibl')
      end
  end.

Lemma Tree_pop_min_minimal :
  forall (ret: t) (tr tr': Tree),
    Heap_Ordered tr -> 
    Tree_is_nonempty_heap tr ->
    Tree_pop_min tr = Some (ret, tr') ->
    Tree_Forall tr (fun (x:t) => ret <= x).
Proof.
  intros.
  destruct tr; simpl in *.
  - contradiction.
  - subst.
    assert (E: ret = val).
    { destruct tr1; inversion H1; subst; auto. }
    subst.
    repeat split.
    + reflexivity.
    + inversion H; subst; auto.
Qed.

Lemma Tree_pop_min_heap :
  forall (ret: t) (tr tr': Tree),
    Heap_Ordered tr ->
    Tree_is_nonempty_heap tr ->
    Tree_pop_min tr = Some (ret, tr') ->
    Heap_Ordered tr' /\ Tree_is_heap tr'.
Proof.
  intros.
  destruct tr; simpl in *.
  - congruence.
  - subst.
    destruct tr1.
    + inversion H1; subst.
      simpl.
      split; auto.
    + inversion H1; subst.
      inversion H; subst.
      split.
      * apply Tree_merge_sibilings_heap_ordered.
        assumption.
      * {
          induction tr1_2.
          - unfold Tree_merge_sibilings. simpl. reflexivity.
          - simpl.
            destruct tr1_2_2.
            + unfold Tree_merge_node; destruct (val0 <? val); simpl; reflexivity.
            + unfold Tree_merge_node; destruct (val0 <? val); simpl; destruct (Tree_merge_sibilings val1 tr1_2_2_1 tr1_2_2_2); simpl; try tauto.
              destruct (val0 <? val2); simpl; reflexivity.
              destruct (val <? val2); simpl; reflexivity.
        }
Qed.

Lemma Tree_pop_min_union :
  forall (ret: t) (tr tr': Tree),
    Tree_is_nonempty_heap tr ->
    Tree_pop_min tr = Some (ret, tr') ->
    Tree_to_MSet tr  = Tree_to_MSet tr' \u \{ret}.
Proof.
  intros.
  destruct tr; simpl in *.
  - congruence.
  - subst.
    destruct tr1.
    + inversion H0; subst.
      permut_simpl.
    + inversion H0; subst.
      rewrite Tree_merge_sibilings_union.
      simpl.
      permut_simpl.
Qed.


(**
type node = {
  mutable value : int;
  child : contents ref;
  sibling : contents ref;
  parent : contents ref
} and contents = Empty | Nonempty of node

type heap = contents ref
 **)

Fixpoint TreeRepr (tr:Tree) (p:loc) { struct tr } : hprop :=
  match tr with
  | Leaf => p ~~> Empty
  | Node x lt rt =>
      \exists (qlt qrt qpar:loc),
          p ~~~> `{ value':= x; child' := qlt; sibling' := qrt; parent' := qpar }
            \* qlt ~>  TreeRepr lt \* qrt ~> TreeRepr rt
  end.

Definition Repr (tr:Tree) (p:loc) : hprop :=
  (p ~> TreeRepr tr) \* \[Heap_Ordered tr].

Definition Contents (tr:Tree) (c:contents_) : hprop :=
  match c with
  | Empty => \[ tr = Leaf ]
  | Nonempty p => p ~> Repr tr \* \[ tr <> Leaf ]
  end.

Definition Heap (tr:Tree) (p:heap_) : hprop :=
  \exists c, p ~~> c \* Contents tr c.

Lemma Contents_isEmpty : forall (tr:Tree) (c:contents_),
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

Lemma Triple_isEmpty : forall (p: loc) (tr: Tree),
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

Lemma Φ_Leaf :
  Φ Leaf = 0.
Proof. simpl. reflexivity. Qed.

Lemma size_merge_nodes : forall tr1 tr2,
    Tree_is_heap tr1 -> Tree_is_heap tr2 ->
    size (Tree_merge_node tr1 tr2) = size tr1 + size tr2.
Proof.
  intros.
  destruct tr1; destruct tr2; simpl; try lia.
  { unfold Tree_merge_node.
    simpl in *; subst.    
    destruct (val <? val0); simpl; lia.
  }
Qed.

Require Import Ring.

Lemma log_lem:
  forall x y: t,
    0 <= x -> 0 <= y ->
    Z.log2_up (1+x+y) <= Z.log2_up (1+x) + Z.log2_up (1+y) + 1.
Proof.
  intros.
  assert (Z.log2_up ((1+x)*(1+y)) <= Z.log2_up (1+x) + Z.log2_up (1+y)).
  {
    rewrite <- Z.log2_up_mul_above.
    reflexivity.
    all: lia.
  }
  assert ((1+x+y) <= (1+x)*(1+y)).
  {
    lia.
  }  
  assert (Z.log2_up (1 + x + y) <=  Z.log2_up ((1 + x) * (1 + y))).
  {
    apply Z.log2_up_le_mono.
    assumption.
  }
  lia.
Qed.

Lemma size_ge_0 : forall (tr : Tree),
    0 <= size tr.
Proof.
  intros.
  induction tr.
  - reflexivity.
  - simpl in *.
    lia.
Qed.

Hint Resolve size_ge_0.

Lemma Z_le_add_r_elim : forall (a b c : Z),
    a <= b -> a + c <= b + c.
Proof.
  intros. lia.
Qed.

Lemma Z_le_sub_r_elim : forall (a b c : Z),
    a <= b -> a - c <= b - c.
Proof.
  intros.
  lia.
Qed.

Lemma Z_le_add_l_elim : forall (a b c : Z),
    a <= b -> c + a <= c + b.
Proof.
  intros. lia.
Qed.

Lemma DΦ_merge_nodes : forall tr1 tr2,
    Tree_is_nonempty_heap tr1 -> Tree_is_nonempty_heap tr2 ->
    Φ (Tree_merge_node tr1 tr2) - Φ tr1 - Φ tr2 <= Z.log2_up ((size tr1) +  (size tr2)) + 1.
Proof.
  intros.
  unfold Tree_merge_node.
  destruct tr1; destruct tr2; simpl in *.
  - contradiction.
  - contradiction.
  - contradiction.
  - destruct (val <? val0); subst; simpl in *.
    {
      repeat rewrite Z.add_0_r.
      repeat rewrite -> Z.add_assoc.
      ring_simplify.
      assert (  Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + size tr2_1 + size tr1_1) - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1) <=
                  Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + Z.log2_up (1+ size tr1_1) + 1 - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1)).
      {
        repeat apply Z_le_sub_r_elim.
        replace ( Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + size tr2_1 + size tr1_1)) with
          (Z.log2_up (1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + 1 + size tr2_1 + size tr1_1)) by lia.
        replace ( Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + 1) with ( Z.log2_up (1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + 1 + Z.log2_up (1 + 1 + size tr2_1 + size tr1_1)) by lia.
        apply Z_le_add_r_elim.
        apply log_lem; auto.
      }
      eapply Z.le_trans with (m := (Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + 1 - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1))).
      assumption.
      {
        replace ( Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + 1 -
                    Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1)) with ( Z.log2_up (1 + 1 + size tr2_1 + size tr1_1) + 1) by lia.
        replace (1 + 1 + size tr2_1 + size tr1_1) with (1 + size tr1_1 + 1 + size tr2_1) by lia.
        reflexivity.
      }
    }
    {
      repeat rewrite Z.add_0_r.
      repeat rewrite -> Z.add_assoc.
      ring_simplify.
      assert (  Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + size tr1_1 + size tr2_1) - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1) <=
                  Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + Z.log2_up (1+ size tr2_1) + 1 - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1)).
      {
        repeat apply Z_le_sub_r_elim.
        replace ( Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + size tr1_1 + size tr2_1)) with
          (Z.log2_up (1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + 1 + size tr1_1 + size tr2_1)) by lia.
        replace ( Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + 1) with ( Z.log2_up (1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + 1 + Z.log2_up (1 + 1 + size tr1_1 + size tr2_1)) by lia.
        apply Z_le_add_r_elim.
        apply log_lem; auto.
      }
      eapply Z.le_trans with (m := (Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + 1 - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1))).
      assumption.
      {
        replace ( Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + Z.log2_up (1 + size tr1_1) + Z.log2_up (1 + size tr2_1) + 1 - Z.log2_up (1 + size tr1_1) - Z.log2_up (1 + size tr2_1)) with ( Z.log2_up (1 + 1 + size tr1_1 + size tr2_1) + 1) by lia.
        replace (1 + 1 + size tr1_1 + size tr2_1) with (1 + size tr1_1 + 1 + size tr2_1) by lia.
        reflexivity.
      }
    }
Qed.

Lemma DΦ_insert : forall tr x,
    Tree_is_heap tr ->
    Φ (Tree_insert tr x) - Φ tr <= Z.log2_up (size tr + 1).
Proof.
  intros.
  unfold Tree_insert.
  destruct tr; simpl.
  - repeat rewrite Z.add_0_r.
    rewrite Z.sub_0_r.
    rewrite Z.add_0_l.
    reflexivity.
  - unfold Tree_merge_node.
    simpl in *; subst.
    destruct (val <? x); simpl.
    all: ring_simplify;
      repeat rewrite Z.add_0_r;
      repeat rewrite -> Z.add_assoc;
      replace (1 + size tr1 + 1) with (1 + 1 + size tr1) by lia;
      lia.
Qed.
  
Lemma TreeRepr_Leaf : forall p,
    (p ~> TreeRepr Leaf) = p ~~> Empty.
Proof.
  auto.
Qed.

Lemma TreeRepr_Node : forall p x lt rt,
    (p ~> TreeRepr (Node x lt rt)) =
      \exists (p1 p2 p3:loc),
          p ~~~> `{ value' := x; child' := p1; sibling' := p2; parent' := p3}
            \* (p1 ~> TreeRepr lt) \* (p2 ~> TreeRepr rt).
Proof.
  auto.
Qed.

Hint Unfold TreeRepr_Leaf TreeRepr_Node.

(*
Lemma Triple_merge_nodes : forall (q1 q2: loc) (tr1 tr2: Tree) (x1 x2: t) (lt1 rt1 lt2 rt2: Tree),
  (tr1 = Node x1 lt1 rt1) -> (tr2 = Node x2 lt2 rt2) ->
  Tree_is_root tr1 -> Tree_is_root tr2 ->                            
  x1 < x2 ->
  SPEC (merge_nodes q1 q2)
    PRE (q1 ~> MHeap tr1) \* (q2 ~> MHeap tr2)
    POST (fun qret => qret ~> Heap (Tree_merge_node tr1 tr2)).
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
*)
