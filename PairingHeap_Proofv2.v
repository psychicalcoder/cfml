Set Implicit Arguments.
From CFML Require Import WPLib.
From CFML Require Import Stdlib.
Require Import PairingHeap_ml.
From TLC Require Import LibMultiset.
Open Scope Z_scope.


Notation t := Z.


Inductive Tree :=
| Leaf
| Node (val : t) (child : Tree) (sibling : Tree) (Parent : Tree).


Fixpoint size (tree : Tree) : Z :=
  match tree with
  | Leaf => 0
  | Node val child sib par => 1 + size sib + size child
  end.


Fixpoint length (tree : Tree) : Z :=
  match tree with
  | Leaf => 0
  | Node _ _ rt _ => 1 + length rt
  end.

Fixpoint Φ (tree : Tree) : Z:=
  match tree with
  | Leaf => 0
  | Node val lt rt par => Z.log2_up (size (Node val lt rt par)) + Φ lt + Φ rt
  end.

Fixpoint Tree_Forall (tree: Tree) (p : t -> Prop) : Prop :=
  match tree with
  | Leaf => True
  | Node val lt rt par => p val /\ Tree_Forall lt p /\ Tree_Forall rt p
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
  | Node x lt rt par => \{ x } \u (Tree_to_MSet lt) \u (Tree_to_MSet rt)
  end.

Inductive Heap_Ordered : Tree -> Prop :=
| Heap_Ordered_leaf: Heap_Ordered Leaf
| Heap_Ordered_node:
    forall val lt rt par,
      Heap_Ordered lt
      -> Heap_Ordered rt
      -> Tree_Forall lt (fun x => val <= x)
      -> Heap_Ordered (Node val lt rt par).

Hint Resolve Heap_Ordered_leaf Heap_Ordered_node.


Require Import Coq.ZArith.BinInt.

Definition Tree_is_nonempty_heap (tree : Tree) :=
  match tree with
  | Leaf => False
  | Node val lt rt _ => rt = Leaf
  end.

Definition Tree_is_heap (tree : Tree) :=
  match tree with
  | Leaf => True
  | Node val lt rt _ => rt = Leaf
  end.

Definition Tree_merge_node (t1 t2: Tree) : Tree :=
  match (t1, t2) with
  | (Leaf, _) => t2
  | (_, Leaf) => t1
  | (Node x1 lt1 rt1 _, Node x2 lt2 rt2 _) =>
      if x1 <? x2 then
        Node x1 (Node x2 lt2 lt1 t1) Leaf Leaf
      else
        Node x2 (Node x1 lt1 lt2 t2) Leaf Leaf
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

Lemma Tree_merge_node_union : forall (tr1 tr2: Tree),
    Tree_is_heap tr1 -> Tree_is_heap tr2 ->
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
  | Leaf => Node x Leaf Leaf Leaf
  | Node x' lt rt _ =>
      Tree_merge_node tr (Node x Leaf Leaf Leaf)
  end.

Lemma Tree_insert_heap_ordered : forall tr x,
    Heap_Ordered tr ->
    Tree_is_heap tr ->
    Heap_Ordered (Tree_insert tr x).
Proof.
  intros.
  unfold Tree_insert.
  destruct tr; simpl in *; subst; auto.
  apply Tree_merge_node_heap_ordered with (tr1 := (Node val tr1 Leaf tr3)) (tr2 := (Node x Leaf Leaf Leaf)); simpl in *; auto.
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

Fixpoint Tree_merge_siblings (x: t) (chld sibl: Tree) : Tree :=
  match sibl with
  | Leaf => Node x chld Leaf
  | Node x' chld' sibl' =>
      let q := Tree_merge_node (Node x chld Leaf) (Node x' chld' Leaf) in
      match sibl' with
      | Leaf => q
      | Node x'' chld'' sibl'' =>
          Tree_merge_node q
            (Tree_merge_siblings x'' chld'' sibl'')
      end
  end.

Lemma Tree_merge_siblings_is_heap :
  forall x chld sibl,
    Tree_is_heap (Tree_merge_siblings x chld sibl).
Proof.
  intros.
  destruct sibl as [| x1 t1 [| x2 t21 t22]]; simpl.
  - reflexivity.
  - unfold Tree_merge_node.
    destruct (x <? x1); simpl; reflexivity.
  - unfold Tree_merge_node.
    destruct (x <? x1); destruct (Tree_merge_siblings x2 t21 t22); simpl.
    + reflexivity.
    + destruct (x <? val); simpl; reflexivity.
    + reflexivity.
    + destruct (x1 <? val); simpl; reflexivity.
Qed.


Lemma Tree_merge_siblings_heap_ordered :
  forall x chld sibl,
    Heap_Ordered (Node x chld sibl) ->
    Heap_Ordered (Tree_merge_siblings x chld sibl).
Proof.
  fix IH 3.
  intros.
  destruct sibl as [| x1 t1 [| x2 t21 t22]]; simpl.
  - exact H.
  - apply Tree_merge_node_heap_ordered; simpl; auto.
    + inversion H; subst.
      apply Heap_Ordered_node; auto.
    + inversion H; subst; assumption.
  - apply Tree_merge_node_heap_ordered; simpl; auto.
    + unfold Tree_merge_node; destruct (x <? x1); simpl; reflexivity.
    + apply Tree_merge_siblings_is_heap.
    + inversion H; subst.
      apply Tree_merge_node_heap_ordered; simpl; auto.
      * inversion H4; subst; auto.
    + apply IH.
      inversion H; subst.
      inversion H4; subst; assumption.
Qed.

Lemma Tree_merge_siblings_union :
  forall x chld sibl,
    Tree_to_MSet (Tree_merge_siblings x chld sibl) =
      \{x} \u Tree_to_MSet chld \u Tree_to_MSet sibl.
Proof.
  fix IH 3.
  intros.
  destruct sibl as [| x1 t1 [| x2 t21 t22]]; simpl.
  - reflexivity.
  - rewrite Tree_merge_node_union; simpl; try permut_simpl; auto.
  - rewrite Tree_merge_node_union.
    + rewrite Tree_merge_node_union.
      simpl.
      rewrite IH.
      permut_simpl.
      all: simpl; reflexivity.
    + unfold Tree_merge_node; destruct (x <? x1); simpl; reflexivity.
    + apply Tree_merge_siblings_is_heap.
Qed.

Definition Tree_pop_min (tr: Tree) : option (t * Tree) :=
  match tr with
  | Leaf => None
  | Node x chld sibl =>
      match chld with
      | Leaf => Some (x, Leaf)
      | Node x' chld' sibl' =>
          Some (x, Tree_merge_siblings x' chld' sibl')
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
      * apply Tree_merge_siblings_heap_ordered.
        assumption.
      * {
          induction tr1_2.
          - unfold Tree_merge_siblings. simpl. reflexivity.
          - simpl.
            destruct tr1_2_2.
            + unfold Tree_merge_node; destruct (val0 <? val); simpl; reflexivity.
            + unfold Tree_merge_node; destruct (val0 <? val); simpl; destruct (Tree_merge_siblings val1 tr1_2_2_1 tr1_2_2_2); simpl; try tauto.
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
      rewrite Tree_merge_siblings_union.
      simpl.
      permut_simpl.
Qed.
