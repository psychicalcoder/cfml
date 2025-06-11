Set Implicit Arguments.

Require Coq.ZArith.BinInt TLC.LibLogic TLC.LibRelation TLC.LibInt TLC.LibListZ.

Require CFML.SepBase CFML.SepLifted CFML.WPLifted CFML.WPRecord CFML.WPArray CFML.WPBuiltin.

Require CFML.Stdlib.Array_ml CFML.Stdlib.List_ml CFML.Stdlib.Sys_ml.

Require Import Coq.ZArith.BinIntDef CFML.Semantics CFML.WPHeader.

Delimit Scope Z_scope with Z.

Existing Instance WPHeader.Use_Enc_any.Enc_any | 99.

Definition node_ : _ := CFML.Semantics.loc.

Definition value' : CFML.Semantics.field := (0)%nat.

Definition child' : CFML.Semantics.field := (1)%nat.

Definition sibling' : CFML.Semantics.field := (2)%nat.

Definition parent' : CFML.Semantics.field := (3)%nat.

Inductive contents_ : Type :=
| Empty : contents_
| Nonempty : node_ -> contents_.

Parameter polymorphic_eq_arg_Empty__ :
  CFML.WPBuiltin.polymorphic_eq_arg Empty.

Hint Resolve polymorphic_eq_arg_Empty__ : polymorphic_eq.

Parameter polymorphic_eq_arg_Nonempty__ :
  forall x0__ : node_,
  CFML.WPBuiltin.polymorphic_eq_arg x0__ ->
  CFML.WPBuiltin.polymorphic_eq_arg (@Nonempty x0__).

Hint Resolve polymorphic_eq_arg_Nonempty__ : polymorphic_eq.

Hint Constructors contents_ : typeclass_instances.

Definition heap_ : Type := CFML.Semantics.loc.

Hint Unfold heap_ : typeclass_instances.

Parameter create : CFML.Semantics.val.

Parameter create_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall x0__ : Coq.Init.Datatypes.unit,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_app CFML.Semantics.loc Pervasives_ml.ref (
            Coq.Lists.List.cons (
              @CFML.SepLifted.dyn_make contents_ _ (Empty : contents_)
            ) Coq.Lists.List.nil
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps create (
        Coq.Lists.List.cons (
          @CFML.SepLifted.dyn_make Coq.Init.Datatypes.unit _ x0__
        ) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF create) => WPHeader_Provide create_cf__.

Parameter is_empty : CFML.Semantics.val.

Parameter is_empty_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall p : CFML.Semantics.loc,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_let_trm (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_app contents_ Pervasives_ml.infix_emark__ (
                  Coq.Lists.List.cons (
                    @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                  ) Coq.Lists.List.nil
                )
              )
            )
          ) (
            fun x0__ : contents_ =>
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool Pervasives_ml.infix_eq__ (
                  Coq.Lists.List.cons (
                    @CFML.SepLifted.dyn_make contents_ _ x0__
                  ) (
                    Coq.Lists.List.cons (
                      @CFML.SepLifted.dyn_make contents_ _ (Empty : contents_)
                    ) Coq.Lists.List.nil
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps is_empty (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make CFML.Semantics.loc _ p) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF is_empty) => WPHeader_Provide is_empty_cf__.

Parameter merge_nodes : CFML.Semantics.val.

Parameter merge_nodes_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall q1 : node_,
    forall q2 : node_,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_let_trm (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                  CFML.WPRecord.val_get_field value'
                ) (
                  Coq.Lists.List.cons (@CFML.SepLifted.dyn_make node_ _ q2) Coq.Lists.List.nil
                )
              )
            )
          ) (
            fun x1__ : Coq.ZArith.BinInt.Z =>
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_let_trm (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                        CFML.WPRecord.val_get_field value'
                      ) (
                        Coq.Lists.List.cons (
                          @CFML.SepLifted.dyn_make node_ _ q1
                        ) Coq.Lists.List.nil
                      )
                    )
                  )
                ) (
                  fun x0__ : Coq.ZArith.BinInt.Z =>
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_if (
                        (fun x__ y__ : Coq.ZArith.BinInt.Z => TLC.LibReflect.isTrue (@TLC.LibOrder.lt _ (@TLC.LibOrder.lt_of_le Coq.ZArith.BinInt.Z TLC.LibInt.le_int_inst) x__ y__)) x0__ x1__
                      ) (
                        @CFML.WPLifted.Wptag (
                          (
                            CFML.WPLifted.Wpgen_let_trm (
                              @CFML.WPLifted.Wptag (
                                (
                                  CFML.WPLifted.Wpgen_app contents_ (
                                    CFML.WPRecord.val_get_field child'
                                  ) (
                                    Coq.Lists.List.cons (
                                      @CFML.SepLifted.dyn_make node_ _ q1
                                    ) Coq.Lists.List.nil
                                  )
                                )
                              )
                            ) (
                              fun x5__ : contents_ =>
                              @CFML.WPLifted.Wptag (
                                (
                                  CFML.WPLifted.Wpgen_let_trm (
                                    @CFML.WPLifted.Wptag (
                                      (
                                        CFML.WPLifted.Wpgen_match x5__ (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_case (
                                                fun A : Type =>
                                                fun EA : CFML.SepLifted.Enc A =>
                                                fun Q :
                                                  A ->
                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                =>
                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                    Coq.Init.Logic.eq x5__ (
                                                      Empty : contents_
                                                    )
                                                  )
                                                ) (
                                                  @CFML.WPLifted.Wptag (
                                                    (
                                                      CFML.WPLifted.Wpgen_val (
                                                        Nonempty q1 : contents_
                                                      )
                                                    )
                                                  ) _ _ Q
                                                )
                                              ) (
                                                CFML.WPLifted.Wpgen_negpat (
                                                  Coq.Init.Logic.not (
                                                    Coq.Init.Logic.eq x5__ (
                                                      Empty : contents_
                                                    )
                                                  )
                                                )
                                              ) (
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_case (
                                                      fun A : Type =>
                                                      fun EA :
                                                        CFML.SepLifted.Enc A
                                                      =>
                                                      fun Q :
                                                        A ->
                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                      =>
                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                        fun p0__ : node_ =>
                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                            Coq.Init.Logic.eq x5__ (
                                                              Nonempty p0__ : contents_
                                                            )
                                                          )
                                                        ) (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_val (
                                                                Empty : contents_
                                                              )
                                                            )
                                                          ) _ _ Q
                                                        )
                                                      )
                                                    ) (
                                                      CFML.WPLifted.Wpgen_negpat (
                                                        forall p0__ : node_,
                                                        Coq.Init.Logic.not (
                                                          Coq.Init.Logic.eq x5__ (
                                                            Nonempty p0__ : contents_
                                                          )
                                                        )
                                                      )
                                                    ) (
                                                      @CFML.WPLifted.Wptag (
                                                        @CFML.WPLifted.Wpgen_done
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ) (
                                    fun x6__ : contents_ =>
                                    @CFML.WPLifted.Wptag (
                                      (
                                        CFML.WPLifted.Wpgen_seq (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                CFML.WPRecord.val_set_field parent'
                                              ) (
                                                Coq.Lists.List.cons (
                                                  @CFML.SepLifted.dyn_make node_ _ q2
                                                ) (
                                                  Coq.Lists.List.cons (
                                                    @CFML.SepLifted.dyn_make contents_ _ x6__
                                                  ) Coq.Lists.List.nil
                                                )
                                              )
                                            )
                                          )
                                        ) (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_let_trm (
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_app contents_ (
                                                      CFML.WPRecord.val_get_field child'
                                                    ) (
                                                      Coq.Lists.List.cons (
                                                        @CFML.SepLifted.dyn_make node_ _ q1
                                                      ) Coq.Lists.List.nil
                                                    )
                                                  )
                                                )
                                              ) (
                                                fun x7__ : contents_ =>
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_seq (
                                                      @CFML.WPLifted.Wptag (
                                                        (
                                                          CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                            CFML.WPRecord.val_set_field sibling'
                                                          ) (
                                                            Coq.Lists.List.cons (
                                                              @CFML.SepLifted.dyn_make node_ _ q2
                                                            ) (
                                                              Coq.Lists.List.cons (
                                                                @CFML.SepLifted.dyn_make contents_ _ x7__
                                                              ) Coq.Lists.List.nil
                                                            )
                                                          )
                                                        )
                                                      )
                                                    ) (
                                                      @CFML.WPLifted.Wptag (
                                                        (
                                                          CFML.WPLifted.Wpgen_seq (
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                  CFML.WPRecord.val_set_field child'
                                                                ) (
                                                                  Coq.Lists.List.cons (
                                                                    @CFML.SepLifted.dyn_make node_ _ q1
                                                                  ) (
                                                                    Coq.Lists.List.cons (
                                                                      @CFML.SepLifted.dyn_make contents_ _ (
                                                                        Nonempty q2 : contents_
                                                                      )
                                                                    ) Coq.Lists.List.nil
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          ) (
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_val q1
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ) (
                        @CFML.WPLifted.Wptag (
                          (
                            CFML.WPLifted.Wpgen_let_trm (
                              @CFML.WPLifted.Wptag (
                                (
                                  CFML.WPLifted.Wpgen_app contents_ (
                                    CFML.WPRecord.val_get_field child'
                                  ) (
                                    Coq.Lists.List.cons (
                                      @CFML.SepLifted.dyn_make node_ _ q2
                                    ) Coq.Lists.List.nil
                                  )
                                )
                              )
                            ) (
                              fun x2__ : contents_ =>
                              @CFML.WPLifted.Wptag (
                                (
                                  CFML.WPLifted.Wpgen_let_trm (
                                    @CFML.WPLifted.Wptag (
                                      (
                                        CFML.WPLifted.Wpgen_match x2__ (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_case (
                                                fun A : Type =>
                                                fun EA : CFML.SepLifted.Enc A =>
                                                fun Q :
                                                  A ->
                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                =>
                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                    Coq.Init.Logic.eq x2__ (
                                                      Empty : contents_
                                                    )
                                                  )
                                                ) (
                                                  @CFML.WPLifted.Wptag (
                                                    (
                                                      CFML.WPLifted.Wpgen_val (
                                                        Nonempty q2 : contents_
                                                      )
                                                    )
                                                  ) _ _ Q
                                                )
                                              ) (
                                                CFML.WPLifted.Wpgen_negpat (
                                                  Coq.Init.Logic.not (
                                                    Coq.Init.Logic.eq x2__ (
                                                      Empty : contents_
                                                    )
                                                  )
                                                )
                                              ) (
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_case (
                                                      fun A : Type =>
                                                      fun EA :
                                                        CFML.SepLifted.Enc A
                                                      =>
                                                      fun Q :
                                                        A ->
                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                      =>
                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                        fun p0__ : node_ =>
                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                            Coq.Init.Logic.eq x2__ (
                                                              Nonempty p0__ : contents_
                                                            )
                                                          )
                                                        ) (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_val (
                                                                Empty : contents_
                                                              )
                                                            )
                                                          ) _ _ Q
                                                        )
                                                      )
                                                    ) (
                                                      CFML.WPLifted.Wpgen_negpat (
                                                        forall p0__ : node_,
                                                        Coq.Init.Logic.not (
                                                          Coq.Init.Logic.eq x2__ (
                                                            Nonempty p0__ : contents_
                                                          )
                                                        )
                                                      )
                                                    ) (
                                                      @CFML.WPLifted.Wptag (
                                                        @CFML.WPLifted.Wpgen_done
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ) (
                                    fun x3__ : contents_ =>
                                    @CFML.WPLifted.Wptag (
                                      (
                                        CFML.WPLifted.Wpgen_seq (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                CFML.WPRecord.val_set_field parent'
                                              ) (
                                                Coq.Lists.List.cons (
                                                  @CFML.SepLifted.dyn_make node_ _ q1
                                                ) (
                                                  Coq.Lists.List.cons (
                                                    @CFML.SepLifted.dyn_make contents_ _ x3__
                                                  ) Coq.Lists.List.nil
                                                )
                                              )
                                            )
                                          )
                                        ) (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_let_trm (
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_app contents_ (
                                                      CFML.WPRecord.val_get_field child'
                                                    ) (
                                                      Coq.Lists.List.cons (
                                                        @CFML.SepLifted.dyn_make node_ _ q2
                                                      ) Coq.Lists.List.nil
                                                    )
                                                  )
                                                )
                                              ) (
                                                fun x4__ : contents_ =>
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_seq (
                                                      @CFML.WPLifted.Wptag (
                                                        (
                                                          CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                            CFML.WPRecord.val_set_field sibling'
                                                          ) (
                                                            Coq.Lists.List.cons (
                                                              @CFML.SepLifted.dyn_make node_ _ q1
                                                            ) (
                                                              Coq.Lists.List.cons (
                                                                @CFML.SepLifted.dyn_make contents_ _ x4__
                                                              ) Coq.Lists.List.nil
                                                            )
                                                          )
                                                        )
                                                      )
                                                    ) (
                                                      @CFML.WPLifted.Wptag (
                                                        (
                                                          CFML.WPLifted.Wpgen_seq (
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                  CFML.WPRecord.val_set_field child'
                                                                ) (
                                                                  Coq.Lists.List.cons (
                                                                    @CFML.SepLifted.dyn_make node_ _ q2
                                                                  ) (
                                                                    Coq.Lists.List.cons (
                                                                      @CFML.SepLifted.dyn_make contents_ _ (
                                                                        Nonempty q1 : contents_
                                                                      )
                                                                    ) Coq.Lists.List.nil
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          ) (
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_val q2
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps merge_nodes (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make node_ _ q1) (
          Coq.Lists.List.cons (@CFML.SepLifted.dyn_make node_ _ q2) Coq.Lists.List.nil
        )
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF merge_nodes) => WPHeader_Provide merge_nodes_cf__.

Parameter merge : CFML.Semantics.val.

Parameter merge_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall h1 : contents_,
    forall h2 : contents_,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_match (h1, h2) (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_case (
                  fun A : Type =>
                  fun EA : CFML.SepLifted.Enc A =>
                  fun Q :
                    A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                  =>
                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                    fun p0__ : contents_ =>
                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                        Coq.Init.Logic.eq (h1, h2) ((Empty : contents_), p0__)
                      )
                    ) (
                      @CFML.WPLifted.Wptag ((CFML.WPLifted.Wpgen_val h2)) _ _ Q
                    )
                  )
                ) (
                  CFML.WPLifted.Wpgen_negpat (
                    forall p0__ : contents_,
                    Coq.Init.Logic.not (
                      Coq.Init.Logic.eq (h1, h2) ((Empty : contents_), p0__)
                    )
                  )
                ) (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_case (
                        fun A : Type =>
                        fun EA : CFML.SepLifted.Enc A =>
                        fun Q :
                          A ->
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                        =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                          fun p0__ : contents_ =>
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                              Coq.Init.Logic.eq (h1, h2) (
                                p0__,
                                (Empty : contents_)
                              )
                            )
                          ) (
                            @CFML.WPLifted.Wptag ((CFML.WPLifted.Wpgen_val h1)) _ _ Q
                          )
                        )
                      ) (
                        CFML.WPLifted.Wpgen_negpat (
                          forall p0__ : contents_,
                          Coq.Init.Logic.not (
                            Coq.Init.Logic.eq (h1, h2) (
                              p0__,
                              (Empty : contents_)
                            )
                          )
                        )
                      ) (
                        @CFML.WPLifted.Wptag (
                          (
                            CFML.WPLifted.Wpgen_case (
                              fun A : Type =>
                              fun EA : CFML.SepLifted.Enc A =>
                              fun Q :
                                A ->
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                              =>
                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                fun q1 : node_ =>
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                  fun q2 : node_ =>
                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                      Coq.Init.Logic.eq (h1, h2) (
                                        (Nonempty q1 : contents_),
                                        (Nonempty q2 : contents_)
                                      )
                                    )
                                  ) (
                                    @CFML.WPLifted.Wptag (
                                      (
                                        CFML.WPLifted.Wpgen_let_trm (
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_app node_ merge_nodes (
                                                Coq.Lists.List.cons (
                                                  @CFML.SepLifted.dyn_make node_ _ q1
                                                ) (
                                                  Coq.Lists.List.cons (
                                                    @CFML.SepLifted.dyn_make node_ _ q2
                                                  ) Coq.Lists.List.nil
                                                )
                                              )
                                            )
                                          )
                                        ) (
                                          fun x0__ : node_ =>
                                          @CFML.WPLifted.Wptag (
                                            (
                                              CFML.WPLifted.Wpgen_val (
                                                Nonempty x0__ : contents_
                                              )
                                            )
                                          )
                                        )
                                      )
                                    ) _ _ Q
                                  )
                                )
                              )
                            ) (
                              CFML.WPLifted.Wpgen_negpat (
                                forall q1 : node_,
                                forall q2 : node_,
                                Coq.Init.Logic.not (
                                  Coq.Init.Logic.eq (h1, h2) (
                                    (Nonempty q1 : contents_),
                                    (Nonempty q2 : contents_)
                                  )
                                )
                              )
                            ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_done))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps merge (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make contents_ _ h1) (
          Coq.Lists.List.cons (@CFML.SepLifted.dyn_make contents_ _ h2) Coq.Lists.List.nil
        )
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF merge) => WPHeader_Provide merge_cf__.

Parameter insert : CFML.Semantics.val.

Parameter insert_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall p : CFML.Semantics.loc,
    forall x : Coq.ZArith.BinInt.Z,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_let_trm (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPRecord.Wpgen_record_new (
                  fun q2 : CFML.Semantics.loc =>
                  Coq.Lists.List.cons (
                    value',
                    @CFML.SepLifted.dyn_make Coq.ZArith.BinInt.Z _ x
                  ) (
                    Coq.Lists.List.cons (
                      child',
                      @CFML.SepLifted.dyn_make contents_ _ (Empty : contents_)
                    ) (
                      Coq.Lists.List.cons (
                        sibling',
                        @CFML.SepLifted.dyn_make contents_ _ (Empty : contents_)
                      ) (
                        Coq.Lists.List.cons (
                          parent',
                          @CFML.SepLifted.dyn_make contents_ _ (
                            Empty : contents_
                          )
                        ) Coq.Lists.List.nil
                      )
                    )
                  )
                )
              )
            )
          ) (
            fun q2 : node_ =>
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_let_trm (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_app contents_ Pervasives_ml.infix_emark__ (
                        Coq.Lists.List.cons (
                          @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                        ) Coq.Lists.List.nil
                      )
                    )
                  )
                ) (
                  fun x0__ : contents_ =>
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_match x0__ (
                        @CFML.WPLifted.Wptag (
                          (
                            CFML.WPLifted.Wpgen_case (
                              fun A : Type =>
                              fun EA : CFML.SepLifted.Enc A =>
                              fun Q :
                                A ->
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                              =>
                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                  Coq.Init.Logic.eq x0__ (Empty : contents_)
                                )
                              ) (
                                @CFML.WPLifted.Wptag (
                                  (
                                    CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit Pervasives_ml.infix_colon_eq__ (
                                      Coq.Lists.List.cons (
                                        @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                                      ) (
                                        Coq.Lists.List.cons (
                                          @CFML.SepLifted.dyn_make contents_ _ (
                                            Nonempty q2 : contents_
                                          )
                                        ) Coq.Lists.List.nil
                                      )
                                    )
                                  )
                                ) _ _ Q
                              )
                            ) (
                              CFML.WPLifted.Wpgen_negpat (
                                Coq.Init.Logic.not (
                                  Coq.Init.Logic.eq x0__ (Empty : contents_)
                                )
                              )
                            ) (
                              @CFML.WPLifted.Wptag (
                                (
                                  CFML.WPLifted.Wpgen_case (
                                    fun A : Type =>
                                    fun EA : CFML.SepLifted.Enc A =>
                                    fun Q :
                                      A ->
                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                    =>
                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                      fun q1 : node_ =>
                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                          Coq.Init.Logic.eq x0__ (
                                            Nonempty q1 : contents_
                                          )
                                        )
                                      ) (
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_let_trm (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_app contents_ (
                                                    CFML.WPRecord.val_get_field child'
                                                  ) (
                                                    Coq.Lists.List.cons (
                                                      @CFML.SepLifted.dyn_make node_ _ q1
                                                    ) Coq.Lists.List.nil
                                                  )
                                                )
                                              )
                                            ) (
                                              fun x1__ : contents_ =>
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_let_trm (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_app CFML.Semantics.loc Pervasives_ml.ref (
                                                          Coq.Lists.List.cons (
                                                            @CFML.SepLifted.dyn_make contents_ _ x1__
                                                          ) Coq.Lists.List.nil
                                                        )
                                                      )
                                                    )
                                                  ) (
                                                    fun x2__ :
                                                      CFML.Semantics.loc
                                                    =>
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_let_trm (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool is_empty (
                                                                Coq.Lists.List.cons (
                                                                  @CFML.SepLifted.dyn_make CFML.Semantics.loc _ x2__
                                                                ) Coq.Lists.List.nil
                                                              )
                                                            )
                                                          )
                                                        ) (
                                                          fun x3__ :
                                                            Coq.Init.Datatypes.bool
                                                          =>
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_seq (
                                                                @CFML.WPLifted.Wptag (
                                                                  (
                                                                    CFML.WPLifted.Wpgen_if x3__ (
                                                                      @CFML.WPLifted.Wptag (
                                                                        (
                                                                          CFML.WPLifted.Wpgen_let_trm (
                                                                            @CFML.WPLifted.Wptag (
                                                                              (
                                                                                CFML.WPLifted.Wpgen_app contents_ Pervasives_ml.infix_emark__ (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                                                                                  ) Coq.Lists.List.nil
                                                                                )
                                                                              )
                                                                            )
                                                                          ) (
                                                                            fun x4__ :
                                                                              contents_
                                                                            =>
                                                                            @CFML.WPLifted.Wptag (
                                                                              (
                                                                                CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                                  CFML.WPRecord.val_set_field parent'
                                                                                ) (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make node_ _ q2
                                                                                  ) (
                                                                                    Coq.Lists.List.cons (
                                                                                      @CFML.SepLifted.dyn_make contents_ _ x4__
                                                                                    ) Coq.Lists.List.nil
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    ) (
                                                                      @CFML.WPLifted.Wptag (
                                                                        (
                                                                          CFML.WPLifted.Wpgen_val (
                                                                            Coq.Init.Datatypes.tt : Coq.Init.Datatypes.unit
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              ) (
                                                                @CFML.WPLifted.Wptag (
                                                                  (
                                                                    CFML.WPLifted.Wpgen_let_trm (
                                                                      @CFML.WPLifted.Wptag (
                                                                        (
                                                                          CFML.WPLifted.Wpgen_app node_ merge_nodes (
                                                                            Coq.Lists.List.cons (
                                                                              @CFML.SepLifted.dyn_make node_ _ q1
                                                                            ) (
                                                                              Coq.Lists.List.cons (
                                                                                @CFML.SepLifted.dyn_make node_ _ q2
                                                                              ) Coq.Lists.List.nil
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    ) (
                                                                      fun x5__ :
                                                                        node_
                                                                      =>
                                                                      @CFML.WPLifted.Wptag (
                                                                        (
                                                                          CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit Pervasives_ml.infix_colon_eq__ (
                                                                            Coq.Lists.List.cons (
                                                                              @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                                                                            ) (
                                                                              Coq.Lists.List.cons (
                                                                                @CFML.SepLifted.dyn_make contents_ _ (
                                                                                  Nonempty x5__ : contents_
                                                                                )
                                                                              ) Coq.Lists.List.nil
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        ) _ _ Q
                                      )
                                    )
                                  ) (
                                    CFML.WPLifted.Wpgen_negpat (
                                      forall q1 : node_,
                                      Coq.Init.Logic.not (
                                        Coq.Init.Logic.eq x0__ (
                                          Nonempty q1 : contents_
                                        )
                                      )
                                    )
                                  ) (
                                    @CFML.WPLifted.Wptag (
                                      @CFML.WPLifted.Wpgen_done
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps insert (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make CFML.Semantics.loc _ p) (
          Coq.Lists.List.cons (@CFML.SepLifted.dyn_make Coq.ZArith.BinInt.Z _ x) Coq.Lists.List.nil
        )
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF insert) => WPHeader_Provide insert_cf__.

Parameter merge_siblings : CFML.Semantics.val.

Parameter merge_siblings_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall q : node_,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_let_trm (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_app contents_ (
                  CFML.WPRecord.val_get_field sibling'
                ) (
                  Coq.Lists.List.cons (@CFML.SepLifted.dyn_make node_ _ q) Coq.Lists.List.nil
                )
              )
            )
          ) (
            fun x0__ : contents_ =>
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_match x0__ (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_case (
                        fun A : Type =>
                        fun EA : CFML.SepLifted.Enc A =>
                        fun Q :
                          A ->
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                        =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                            Coq.Init.Logic.eq x0__ (Empty : contents_)
                          )
                        ) (
                          @CFML.WPLifted.Wptag ((CFML.WPLifted.Wpgen_val q)) _ _ Q
                        )
                      ) (
                        CFML.WPLifted.Wpgen_negpat (
                          Coq.Init.Logic.not (
                            Coq.Init.Logic.eq x0__ (Empty : contents_)
                          )
                        )
                      ) (
                        @CFML.WPLifted.Wptag (
                          (
                            CFML.WPLifted.Wpgen_case (
                              fun A : Type =>
                              fun EA : CFML.SepLifted.Enc A =>
                              fun Q :
                                A ->
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                              =>
                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                fun q1 : node_ =>
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                    Coq.Init.Logic.eq x0__ (
                                      Nonempty q1 : contents_
                                    )
                                  )
                                ) (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_let_trm (
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_app node_ merge_nodes (
                                              Coq.Lists.List.cons (
                                                @CFML.SepLifted.dyn_make node_ _ q
                                              ) (
                                                Coq.Lists.List.cons (
                                                  @CFML.SepLifted.dyn_make node_ _ q1
                                                ) Coq.Lists.List.nil
                                              )
                                            )
                                          )
                                        )
                                      ) (
                                        fun q2 : node_ =>
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_let_trm (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_app contents_ (
                                                    CFML.WPRecord.val_get_field sibling'
                                                  ) (
                                                    Coq.Lists.List.cons (
                                                      @CFML.SepLifted.dyn_make node_ _ q1
                                                    ) Coq.Lists.List.nil
                                                  )
                                                )
                                              )
                                            ) (
                                              fun x1__ : contents_ =>
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_match x1__ (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_case (
                                                          fun A : Type =>
                                                          fun EA :
                                                            CFML.SepLifted.Enc A
                                                          =>
                                                          fun Q :
                                                            A ->
                                                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                          =>
                                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                              Coq.Init.Logic.eq x1__ (
                                                                Empty : contents_
                                                              )
                                                            )
                                                          ) (
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_val q2
                                                              )
                                                            ) _ _ Q
                                                          )
                                                        ) (
                                                          CFML.WPLifted.Wpgen_negpat (
                                                            Coq.Init.Logic.not (
                                                              Coq.Init.Logic.eq x1__ (
                                                                Empty : contents_
                                                              )
                                                            )
                                                          )
                                                        ) (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_case (
                                                                fun A : Type =>
                                                                fun EA :
                                                                  CFML.SepLifted.Enc A
                                                                =>
                                                                fun Q :
                                                                  A ->
                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                =>
                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                  fun q3 :
                                                                    node_
                                                                  =>
                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                      Coq.Init.Logic.eq x1__ (
                                                                        Nonempty q3 : contents_
                                                                      )
                                                                    )
                                                                  ) (
                                                                    @CFML.WPLifted.Wptag (
                                                                      (
                                                                        CFML.WPLifted.Wpgen_let_trm (
                                                                          @CFML.WPLifted.Wptag (
                                                                            (
                                                                              CFML.WPLifted.Wpgen_app node_ merge_siblings (
                                                                                Coq.Lists.List.cons (
                                                                                  @CFML.SepLifted.dyn_make node_ _ q3
                                                                                ) Coq.Lists.List.nil
                                                                              )
                                                                            )
                                                                          )
                                                                        ) (
                                                                          fun x2__ :
                                                                            node_
                                                                          =>
                                                                          @CFML.WPLifted.Wptag (
                                                                            (
                                                                              CFML.WPLifted.Wpgen_app node_ merge_nodes (
                                                                                Coq.Lists.List.cons (
                                                                                  @CFML.SepLifted.dyn_make node_ _ q2
                                                                                ) (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make node_ _ x2__
                                                                                  ) Coq.Lists.List.nil
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    ) _ _ Q
                                                                  )
                                                                )
                                                              ) (
                                                                CFML.WPLifted.Wpgen_negpat (
                                                                  forall q3 :
                                                                    node_,
                                                                  Coq.Init.Logic.not (
                                                                    Coq.Init.Logic.eq x1__ (
                                                                      Nonempty q3 : contents_
                                                                    )
                                                                  )
                                                                )
                                                              ) (
                                                                @CFML.WPLifted.Wptag (
                                                                  @CFML.WPLifted.Wpgen_done
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ) _ _ Q
                                )
                              )
                            ) (
                              CFML.WPLifted.Wpgen_negpat (
                                forall q1 : node_,
                                Coq.Init.Logic.not (
                                  Coq.Init.Logic.eq x0__ (
                                    Nonempty q1 : contents_
                                  )
                                )
                              )
                            ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_done))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps merge_siblings (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make node_ _ q) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF merge_siblings) => WPHeader_Provide merge_siblings_cf__.

Parameter pop_min : CFML.Semantics.val.

Parameter pop_min_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall p : CFML.Semantics.loc,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_let_trm (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_app contents_ Pervasives_ml.infix_emark__ (
                  Coq.Lists.List.cons (
                    @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                  ) Coq.Lists.List.nil
                )
              )
            )
          ) (
            fun x0__ : contents_ =>
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_match x0__ (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_case (
                        fun A : Type =>
                        fun EA : CFML.SepLifted.Enc A =>
                        fun Q :
                          A ->
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                        =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                            Coq.Init.Logic.eq x0__ (Empty : contents_)
                          )
                        ) (
                          @CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_fail) _ _ Q
                        )
                      ) (
                        CFML.WPLifted.Wpgen_negpat (
                          Coq.Init.Logic.not (
                            Coq.Init.Logic.eq x0__ (Empty : contents_)
                          )
                        )
                      ) (
                        @CFML.WPLifted.Wptag (
                          (
                            CFML.WPLifted.Wpgen_case (
                              fun A : Type =>
                              fun EA : CFML.SepLifted.Enc A =>
                              fun Q :
                                A ->
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                              =>
                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                fun q : node_ =>
                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                    Coq.Init.Logic.eq x0__ (
                                      Nonempty q : contents_
                                    )
                                  )
                                ) (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_let_trm (
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                              CFML.WPRecord.val_get_field value'
                                            ) (
                                              Coq.Lists.List.cons (
                                                @CFML.SepLifted.dyn_make node_ _ q
                                              ) Coq.Lists.List.nil
                                            )
                                          )
                                        )
                                      ) (
                                        fun x : Coq.ZArith.BinInt.Z =>
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_let_trm (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_app contents_ (
                                                    CFML.WPRecord.val_get_field child'
                                                  ) (
                                                    Coq.Lists.List.cons (
                                                      @CFML.SepLifted.dyn_make node_ _ q
                                                    ) Coq.Lists.List.nil
                                                  )
                                                )
                                              )
                                            ) (
                                              fun x1__ : contents_ =>
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_seq (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_match x1__ (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_case (
                                                                fun A : Type =>
                                                                fun EA :
                                                                  CFML.SepLifted.Enc A
                                                                =>
                                                                fun Q :
                                                                  A ->
                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                =>
                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                    Coq.Init.Logic.eq x1__ (
                                                                      Empty : contents_
                                                                    )
                                                                  )
                                                                ) (
                                                                  @CFML.WPLifted.Wptag (
                                                                    (
                                                                      CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit Pervasives_ml.infix_colon_eq__ (
                                                                        Coq.Lists.List.cons (
                                                                          @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                                                                        ) (
                                                                          Coq.Lists.List.cons (
                                                                            @CFML.SepLifted.dyn_make contents_ _ (
                                                                              Empty : contents_
                                                                            )
                                                                          ) Coq.Lists.List.nil
                                                                        )
                                                                      )
                                                                    )
                                                                  ) _ _ Q
                                                                )
                                                              ) (
                                                                CFML.WPLifted.Wpgen_negpat (
                                                                  Coq.Init.Logic.not (
                                                                    Coq.Init.Logic.eq x1__ (
                                                                      Empty : contents_
                                                                    )
                                                                  )
                                                                )
                                                              ) (
                                                                @CFML.WPLifted.Wptag (
                                                                  (
                                                                    CFML.WPLifted.Wpgen_case (
                                                                      fun A :
                                                                        Type
                                                                      =>
                                                                      fun EA :
                                                                        CFML.SepLifted.Enc A
                                                                      =>
                                                                      fun Q :
                                                                        A ->
                                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                      =>
                                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                        fun child :
                                                                          node_
                                                                        =>
                                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                            Coq.Init.Logic.eq x1__ (
                                                                              Nonempty child : contents_
                                                                            )
                                                                          )
                                                                        ) (
                                                                          @CFML.WPLifted.Wptag (
                                                                            (
                                                                              CFML.WPLifted.Wpgen_let_trm (
                                                                                @CFML.WPLifted.Wptag (
                                                                                  (
                                                                                    CFML.WPLifted.Wpgen_app node_ merge_siblings (
                                                                                      Coq.Lists.List.cons (
                                                                                        @CFML.SepLifted.dyn_make node_ _ child
                                                                                      ) Coq.Lists.List.nil
                                                                                    )
                                                                                  )
                                                                                )
                                                                              ) (
                                                                                fun x2__ :
                                                                                  node_
                                                                                =>
                                                                                @CFML.WPLifted.Wptag (
                                                                                  (
                                                                                    CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit Pervasives_ml.infix_colon_eq__ (
                                                                                      Coq.Lists.List.cons (
                                                                                        @CFML.SepLifted.dyn_make CFML.Semantics.loc _ p
                                                                                      ) (
                                                                                        Coq.Lists.List.cons (
                                                                                          @CFML.SepLifted.dyn_make contents_ _ (
                                                                                            Nonempty x2__ : contents_
                                                                                          )
                                                                                        ) Coq.Lists.List.nil
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          ) _ _ Q
                                                                        )
                                                                      )
                                                                    ) (
                                                                      CFML.WPLifted.Wpgen_negpat (
                                                                        forall child :
                                                                          node_,
                                                                        Coq.Init.Logic.not (
                                                                          Coq.Init.Logic.eq x1__ (
                                                                            Nonempty child : contents_
                                                                          )
                                                                        )
                                                                      )
                                                                    ) (
                                                                      @CFML.WPLifted.Wptag (
                                                                        @CFML.WPLifted.Wpgen_done
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  ) (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_val x
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  ) _ _ Q
                                )
                              )
                            ) (
                              CFML.WPLifted.Wpgen_negpat (
                                forall q : node_,
                                Coq.Init.Logic.not (
                                  Coq.Init.Logic.eq x0__ (
                                    Nonempty q : contents_
                                  )
                                )
                              )
                            ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_done))
                          )
                        )
                      )
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps pop_min (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make CFML.Semantics.loc _ p) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF pop_min) => WPHeader_Provide pop_min_cf__.

Parameter find_parent : CFML.Semantics.val.

Parameter find_parent_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall k : contents_,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_match k (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_case (
                  fun A : Type =>
                  fun EA : CFML.SepLifted.Enc A =>
                  fun Q :
                    A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                  =>
                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                      Coq.Init.Logic.eq k (Empty : contents_)
                    )
                  ) (
                    @CFML.WPLifted.Wptag (
                      (CFML.WPLifted.Wpgen_val (Empty : contents_))
                    ) _ _ Q
                  )
                ) (
                  CFML.WPLifted.Wpgen_negpat (
                    Coq.Init.Logic.not (Coq.Init.Logic.eq k (Empty : contents_))
                  )
                ) (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_case (
                        fun A : Type =>
                        fun EA : CFML.SepLifted.Enc A =>
                        fun Q :
                          A ->
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                        =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                          fun q : node_ =>
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                              Coq.Init.Logic.eq k (Nonempty q : contents_)
                            )
                          ) (
                            @CFML.WPLifted.Wptag (
                              (
                                CFML.WPLifted.Wpgen_let_trm (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_app contents_ (
                                        CFML.WPRecord.val_get_field sibling'
                                      ) (
                                        Coq.Lists.List.cons (
                                          @CFML.SepLifted.dyn_make node_ _ q
                                        ) Coq.Lists.List.nil
                                      )
                                    )
                                  )
                                ) (
                                  fun x0__ : contents_ =>
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_let_trm (
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool Pervasives_ml.infix_eq__ (
                                              Coq.Lists.List.cons (
                                                @CFML.SepLifted.dyn_make contents_ _ x0__
                                              ) (
                                                Coq.Lists.List.cons (
                                                  @CFML.SepLifted.dyn_make contents_ _ (
                                                    Empty : contents_
                                                  )
                                                ) Coq.Lists.List.nil
                                              )
                                            )
                                          )
                                        )
                                      ) (
                                        fun x1__ : Coq.Init.Datatypes.bool =>
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_if x1__ (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_app contents_ (
                                                    CFML.WPRecord.val_get_field parent'
                                                  ) (
                                                    Coq.Lists.List.cons (
                                                      @CFML.SepLifted.dyn_make node_ _ q
                                                    ) Coq.Lists.List.nil
                                                  )
                                                )
                                              )
                                            ) (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_let_trm (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_app contents_ (
                                                          CFML.WPRecord.val_get_field sibling'
                                                        ) (
                                                          Coq.Lists.List.cons (
                                                            @CFML.SepLifted.dyn_make node_ _ q
                                                          ) Coq.Lists.List.nil
                                                        )
                                                      )
                                                    )
                                                  ) (
                                                    fun x2__ : contents_ =>
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_app contents_ find_parent (
                                                          Coq.Lists.List.cons (
                                                            @CFML.SepLifted.dyn_make contents_ _ x2__
                                                          ) Coq.Lists.List.nil
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ) _ _ Q
                          )
                        )
                      ) (
                        CFML.WPLifted.Wpgen_negpat (
                          forall q : node_,
                          Coq.Init.Logic.not (
                            Coq.Init.Logic.eq k (Nonempty q : contents_)
                          )
                        )
                      ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_done))
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps find_parent (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make contents_ _ k) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF find_parent) => WPHeader_Provide find_parent_cf__.

Parameter root : CFML.Semantics.val.

Parameter root_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall k : contents_,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_match k (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_case (
                  fun A : Type =>
                  fun EA : CFML.SepLifted.Enc A =>
                  fun Q :
                    A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                  =>
                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                      Coq.Init.Logic.eq k (Empty : contents_)
                    )
                  ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_fail) _ _ Q)
                ) (
                  CFML.WPLifted.Wpgen_negpat (
                    Coq.Init.Logic.not (Coq.Init.Logic.eq k (Empty : contents_))
                  )
                ) (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_case (
                        fun A : Type =>
                        fun EA : CFML.SepLifted.Enc A =>
                        fun Q :
                          A ->
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                        =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                          fun q : node_ =>
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                              Coq.Init.Logic.eq k (Nonempty q : contents_)
                            )
                          ) (
                            @CFML.WPLifted.Wptag (
                              (
                                CFML.WPLifted.Wpgen_let_trm (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_app contents_ (
                                        CFML.WPRecord.val_get_field sibling'
                                      ) (
                                        Coq.Lists.List.cons (
                                          @CFML.SepLifted.dyn_make node_ _ q
                                        ) Coq.Lists.List.nil
                                      )
                                    )
                                  )
                                ) (
                                  fun x0__ : contents_ =>
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_let_trm (
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool Pervasives_ml.infix_eq__ (
                                              Coq.Lists.List.cons (
                                                @CFML.SepLifted.dyn_make contents_ _ x0__
                                              ) (
                                                Coq.Lists.List.cons (
                                                  @CFML.SepLifted.dyn_make contents_ _ (
                                                    Empty : contents_
                                                  )
                                                ) Coq.Lists.List.nil
                                              )
                                            )
                                          )
                                        )
                                      ) (
                                        fun x1__ : Coq.Init.Datatypes.bool =>
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_let_trm (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_if x1__ (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_let_trm (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_app contents_ (
                                                                CFML.WPRecord.val_get_field parent'
                                                              ) (
                                                                Coq.Lists.List.cons (
                                                                  @CFML.SepLifted.dyn_make node_ _ q
                                                                ) Coq.Lists.List.nil
                                                              )
                                                            )
                                                          )
                                                        ) (
                                                          fun x4__ :
                                                            contents_
                                                          =>
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool Pervasives_ml.infix_eq__ (
                                                                Coq.Lists.List.cons (
                                                                  @CFML.SepLifted.dyn_make contents_ _ x4__
                                                                ) (
                                                                  Coq.Lists.List.cons (
                                                                    @CFML.SepLifted.dyn_make contents_ _ (
                                                                      Empty : contents_
                                                                    )
                                                                  ) Coq.Lists.List.nil
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  ) (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_val (
                                                          Coq.Init.Datatypes.false : Coq.Init.Datatypes.bool
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            ) (
                                              fun x5__ :
                                                Coq.Init.Datatypes.bool
                                              =>
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_if x5__ (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_val k
                                                      )
                                                    )
                                                  ) (
                                                    @CFML.WPLifted.Wptag (
                                                      (
                                                        CFML.WPLifted.Wpgen_let_trm (
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_app contents_ (
                                                                CFML.WPRecord.val_get_field parent'
                                                              ) (
                                                                Coq.Lists.List.cons (
                                                                  @CFML.SepLifted.dyn_make node_ _ q
                                                                ) Coq.Lists.List.nil
                                                              )
                                                            )
                                                          )
                                                        ) (
                                                          fun x6__ :
                                                            contents_
                                                          =>
                                                          @CFML.WPLifted.Wptag (
                                                            (
                                                              CFML.WPLifted.Wpgen_let_trm (
                                                                @CFML.WPLifted.Wptag (
                                                                  (
                                                                    CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool Pervasives_ml.infix_eq__ (
                                                                      Coq.Lists.List.cons (
                                                                        @CFML.SepLifted.dyn_make contents_ _ x6__
                                                                      ) (
                                                                        Coq.Lists.List.cons (
                                                                          @CFML.SepLifted.dyn_make contents_ _ (
                                                                            Empty : contents_
                                                                          )
                                                                        ) Coq.Lists.List.nil
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              ) (
                                                                fun x7__ :
                                                                  Coq.Init.Datatypes.bool
                                                                =>
                                                                @CFML.WPLifted.Wptag (
                                                                  (
                                                                    CFML.WPLifted.Wpgen_if x7__ (
                                                                      @CFML.WPLifted.Wptag (
                                                                        (
                                                                          CFML.WPLifted.Wpgen_let_trm (
                                                                            @CFML.WPLifted.Wptag (
                                                                              (
                                                                                CFML.WPLifted.Wpgen_app contents_ (
                                                                                  CFML.WPRecord.val_get_field sibling'
                                                                                ) (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make node_ _ q
                                                                                  ) Coq.Lists.List.nil
                                                                                )
                                                                              )
                                                                            )
                                                                          ) (
                                                                            fun x9__ :
                                                                              contents_
                                                                            =>
                                                                            @CFML.WPLifted.Wptag (
                                                                              (
                                                                                CFML.WPLifted.Wpgen_app contents_ root (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make contents_ _ x9__
                                                                                  ) Coq.Lists.List.nil
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    ) (
                                                                      @CFML.WPLifted.Wptag (
                                                                        (
                                                                          CFML.WPLifted.Wpgen_let_trm (
                                                                            @CFML.WPLifted.Wptag (
                                                                              (
                                                                                CFML.WPLifted.Wpgen_app contents_ (
                                                                                  CFML.WPRecord.val_get_field parent'
                                                                                ) (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make node_ _ q
                                                                                  ) Coq.Lists.List.nil
                                                                                )
                                                                              )
                                                                            )
                                                                          ) (
                                                                            fun x8__ :
                                                                              contents_
                                                                            =>
                                                                            @CFML.WPLifted.Wptag (
                                                                              (
                                                                                CFML.WPLifted.Wpgen_app contents_ root (
                                                                                  Coq.Lists.List.cons (
                                                                                    @CFML.SepLifted.dyn_make contents_ _ x8__
                                                                                  ) Coq.Lists.List.nil
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ) _ _ Q
                          )
                        )
                      ) (
                        CFML.WPLifted.Wpgen_negpat (
                          forall q : node_,
                          Coq.Init.Logic.not (
                            Coq.Init.Logic.eq k (Nonempty q : contents_)
                          )
                        )
                      ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_done))
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps root (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make contents_ _ k) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF root) => WPHeader_Provide root_cf__.

Parameter left_sibling : CFML.Semantics.val.

Parameter left_sibling_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall k : contents_,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_let_fun (
            fun A : Type =>
            fun EA : CFML.SepLifted.Enc A =>
            fun Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop =>
            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
              fun sib_itr : CFML.Semantics.val =>
              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                  CFML.WPLifted.Wpgen_body (
                    forall s : contents_,
                    forall H :
                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
                    forall A : Type,
                    forall EA : CFML.SepLifted.Enc A,
                    forall Q :
                      A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
                      @CFML.WPLifted.Wptag (
                        (
                          CFML.WPLifted.Wpgen_match s (
                            @CFML.WPLifted.Wptag (
                              (
                                CFML.WPLifted.Wpgen_case (
                                  fun A : Type =>
                                  fun EA : CFML.SepLifted.Enc A =>
                                  fun Q :
                                    A ->
                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                  =>
                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                      Coq.Init.Logic.eq s (Empty : contents_)
                                    )
                                  ) (
                                    @CFML.WPLifted.Wptag (
                                      @CFML.WPLifted.Wpgen_fail
                                    ) _ _ Q
                                  )
                                ) (
                                  CFML.WPLifted.Wpgen_negpat (
                                    Coq.Init.Logic.not (
                                      Coq.Init.Logic.eq s (Empty : contents_)
                                    )
                                  )
                                ) (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_case (
                                        fun A : Type =>
                                        fun EA : CFML.SepLifted.Enc A =>
                                        fun Q :
                                          A ->
                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                        =>
                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                          fun q : node_ =>
                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                              Coq.Init.Logic.eq s (
                                                Nonempty q : contents_
                                              )
                                            )
                                          ) (
                                            @CFML.WPLifted.Wptag (
                                              (
                                                CFML.WPLifted.Wpgen_let_trm (
                                                  @CFML.WPLifted.Wptag (
                                                    (
                                                      CFML.WPLifted.Wpgen_app contents_ (
                                                        CFML.WPRecord.val_get_field sibling'
                                                      ) (
                                                        Coq.Lists.List.cons (
                                                          @CFML.SepLifted.dyn_make node_ _ q
                                                        ) Coq.Lists.List.nil
                                                      )
                                                    )
                                                  )
                                                ) (
                                                  fun x0__ : contents_ =>
                                                  @CFML.WPLifted.Wptag (
                                                    (
                                                      CFML.WPLifted.Wpgen_let_trm (
                                                        @CFML.WPLifted.Wptag (
                                                          (
                                                            CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.bool Pervasives_ml.infix_eq__ (
                                                              Coq.Lists.List.cons (
                                                                @CFML.SepLifted.dyn_make contents_ _ x0__
                                                              ) (
                                                                Coq.Lists.List.cons (
                                                                  @CFML.SepLifted.dyn_make contents_ _ k
                                                                ) Coq.Lists.List.nil
                                                              )
                                                            )
                                                          )
                                                        )
                                                      ) (
                                                        fun x1__ :
                                                          Coq.Init.Datatypes.bool
                                                        =>
                                                        @CFML.WPLifted.Wptag (
                                                          (
                                                            CFML.WPLifted.Wpgen_if x1__ (
                                                              @CFML.WPLifted.Wptag (
                                                                (
                                                                  CFML.WPLifted.Wpgen_val s
                                                                )
                                                              )
                                                            ) (
                                                              @CFML.WPLifted.Wptag (
                                                                (
                                                                  CFML.WPLifted.Wpgen_let_trm (
                                                                    @CFML.WPLifted.Wptag (
                                                                      (
                                                                        CFML.WPLifted.Wpgen_app contents_ (
                                                                          CFML.WPRecord.val_get_field sibling'
                                                                        ) (
                                                                          Coq.Lists.List.cons (
                                                                            @CFML.SepLifted.dyn_make node_ _ q
                                                                          ) Coq.Lists.List.nil
                                                                        )
                                                                      )
                                                                    )
                                                                  ) (
                                                                    fun x2__ :
                                                                      contents_
                                                                    =>
                                                                    @CFML.WPLifted.Wptag (
                                                                      (
                                                                        CFML.WPLifted.Wpgen_app contents_ sib_itr (
                                                                          Coq.Lists.List.cons (
                                                                            @CFML.SepLifted.dyn_make contents_ _ x2__
                                                                          ) Coq.Lists.List.nil
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            ) _ _ Q
                                          )
                                        )
                                      ) (
                                        CFML.WPLifted.Wpgen_negpat (
                                          forall q : node_,
                                          Coq.Init.Logic.not (
                                            Coq.Init.Logic.eq s (
                                              Nonempty q : contents_
                                            )
                                          )
                                        )
                                      ) (
                                        @CFML.WPLifted.Wptag (
                                          @CFML.WPLifted.Wpgen_done
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      ) _ _ (
                        fun res__ : _ =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (
                          Q res__
                        ) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
                      )
                    ) ->
                    CFML.SepLifted.Triple (
                      CFML.SepLifted.Trm_apps sib_itr (
                        Coq.Lists.List.cons (
                          @CFML.SepLifted.dyn_make contents_ _ s
                        ) Coq.Lists.List.nil
                      )
                    ) H Q
                  )
                )
              ) (
                @CFML.WPLifted.Wptag (
                  (
                    CFML.WPLifted.Wpgen_let_trm (
                      @CFML.WPLifted.Wptag (
                        (
                          CFML.WPLifted.Wpgen_app contents_ find_parent (
                            Coq.Lists.List.cons (
                              @CFML.SepLifted.dyn_make contents_ _ k
                            ) Coq.Lists.List.nil
                          )
                        )
                      )
                    ) (
                      fun parent : contents_ =>
                      @CFML.WPLifted.Wptag (
                        (
                          CFML.WPLifted.Wpgen_match parent (
                            @CFML.WPLifted.Wptag (
                              (
                                CFML.WPLifted.Wpgen_case (
                                  fun A : Type =>
                                  fun EA : CFML.SepLifted.Enc A =>
                                  fun Q :
                                    A ->
                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                  =>
                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                      Coq.Init.Logic.eq parent (
                                        Empty : contents_
                                      )
                                    )
                                  ) (
                                    @CFML.WPLifted.Wptag (
                                      @CFML.WPLifted.Wpgen_fail
                                    ) _ _ Q
                                  )
                                ) (
                                  CFML.WPLifted.Wpgen_negpat (
                                    Coq.Init.Logic.not (
                                      Coq.Init.Logic.eq parent (
                                        Empty : contents_
                                      )
                                    )
                                  )
                                ) (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_case (
                                        fun A : Type =>
                                        fun EA : CFML.SepLifted.Enc A =>
                                        fun Q :
                                          A ->
                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                        =>
                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                          fun parent_node : node_ =>
                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                              Coq.Init.Logic.eq parent (
                                                Nonempty parent_node : contents_
                                              )
                                            )
                                          ) (
                                            @CFML.WPLifted.Wptag (
                                              (
                                                CFML.WPLifted.Wpgen_let_trm (
                                                  @CFML.WPLifted.Wptag (
                                                    (
                                                      CFML.WPLifted.Wpgen_app contents_ (
                                                        CFML.WPRecord.val_get_field child'
                                                      ) (
                                                        Coq.Lists.List.cons (
                                                          @CFML.SepLifted.dyn_make node_ _ parent_node
                                                        ) Coq.Lists.List.nil
                                                      )
                                                    )
                                                  )
                                                ) (
                                                  fun x3__ : contents_ =>
                                                  @CFML.WPLifted.Wptag (
                                                    (
                                                      CFML.WPLifted.Wpgen_app contents_ sib_itr (
                                                        Coq.Lists.List.cons (
                                                          @CFML.SepLifted.dyn_make contents_ _ x3__
                                                        ) Coq.Lists.List.nil
                                                      )
                                                    )
                                                  )
                                                )
                                              )
                                            ) _ _ Q
                                          )
                                        )
                                      ) (
                                        CFML.WPLifted.Wpgen_negpat (
                                          forall parent_node : node_,
                                          Coq.Init.Logic.not (
                                            Coq.Init.Logic.eq parent (
                                              Nonempty parent_node : contents_
                                            )
                                          )
                                        )
                                      ) (
                                        @CFML.WPLifted.Wptag (
                                          @CFML.WPLifted.Wpgen_done
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            )
                          )
                        )
                      )
                    )
                  )
                ) _ _ Q
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps left_sibling (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make contents_ _ k) Coq.Lists.List.nil
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF left_sibling) => WPHeader_Provide left_sibling_cf__.

Parameter decrease_key : CFML.Semantics.val.

Parameter decrease_key_cf__ :
  CFML.WPLifted.Wpgen_body (
    forall k : contents_,
    forall d : Coq.ZArith.BinInt.Z,
    forall H : CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    forall A : Type,
    forall EA : CFML.SepLifted.Enc A,
    forall Q : A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop,
    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.himpl H (
      @CFML.WPLifted.Wptag (
        (
          CFML.WPLifted.Wpgen_match k (
            @CFML.WPLifted.Wptag (
              (
                CFML.WPLifted.Wpgen_case (
                  fun A : Type =>
                  fun EA : CFML.SepLifted.Enc A =>
                  fun Q :
                    A -> CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                  =>
                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                      Coq.Init.Logic.eq k (Empty : contents_)
                    )
                  ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_fail) _ _ Q)
                ) (
                  CFML.WPLifted.Wpgen_negpat (
                    Coq.Init.Logic.not (Coq.Init.Logic.eq k (Empty : contents_))
                  )
                ) (
                  @CFML.WPLifted.Wptag (
                    (
                      CFML.WPLifted.Wpgen_case (
                        fun A : Type =>
                        fun EA : CFML.SepLifted.Enc A =>
                        fun Q :
                          A ->
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                        =>
                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                          fun q : node_ =>
                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                              Coq.Init.Logic.eq k (Nonempty q : contents_)
                            )
                          ) (
                            @CFML.WPLifted.Wptag (
                              (
                                CFML.WPLifted.Wpgen_let_trm (
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_app contents_ find_parent (
                                        Coq.Lists.List.cons (
                                          @CFML.SepLifted.dyn_make contents_ _ k
                                        ) Coq.Lists.List.nil
                                      )
                                    )
                                  )
                                ) (
                                  fun parent : contents_ =>
                                  @CFML.WPLifted.Wptag (
                                    (
                                      CFML.WPLifted.Wpgen_match parent (
                                        @CFML.WPLifted.Wptag (
                                          (
                                            CFML.WPLifted.Wpgen_case (
                                              fun A : Type =>
                                              fun EA : CFML.SepLifted.Enc A =>
                                              fun Q :
                                                A ->
                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                              =>
                                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                  Coq.Init.Logic.eq parent (
                                                    Empty : contents_
                                                  )
                                                )
                                              ) (
                                                @CFML.WPLifted.Wptag (
                                                  (
                                                    CFML.WPLifted.Wpgen_let_trm (
                                                      @CFML.WPLifted.Wptag (
                                                        (
                                                          CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                            CFML.WPRecord.val_get_field value'
                                                          ) (
                                                            Coq.Lists.List.cons (
                                                              @CFML.SepLifted.dyn_make node_ _ q
                                                            ) Coq.Lists.List.nil
                                                          )
                                                        )
                                                      )
                                                    ) (
                                                      fun x1__ :
                                                        Coq.ZArith.BinInt.Z
                                                      =>
                                                      @CFML.WPLifted.Wptag (
                                                        (
                                                          CFML.WPLifted.Wpgen_let_trm (
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                  CFML.WPRecord.val_get_field value'
                                                                ) (
                                                                  Coq.Lists.List.cons (
                                                                    @CFML.SepLifted.dyn_make node_ _ q
                                                                  ) Coq.Lists.List.nil
                                                                )
                                                              )
                                                            )
                                                          ) (
                                                            fun x0__ :
                                                              Coq.ZArith.BinInt.Z
                                                            =>
                                                            @CFML.WPLifted.Wptag (
                                                              (
                                                                CFML.WPLifted.Wpgen_let_val (
                                                                  (fun x__ y__ : Coq.ZArith.BinInt.Z => TLC.LibReflect.isTrue (Coq.Init.Logic.eq x__ y__)) x0__ (
                                                                    Coq.ZArith.BinInt.Z.sub x1__ d
                                                                  )
                                                                ) (
                                                                  fun x2__ :
                                                                    Coq.Init.Datatypes.bool
                                                                  =>
                                                                  @CFML.WPLifted.Wptag (
                                                                    (
                                                                      CFML.WPLifted.Wpgen_match x2__ (
                                                                        @CFML.WPLifted.Wptag (
                                                                          (
                                                                            CFML.WPLifted.Wpgen_case (
                                                                              fun A :
                                                                                Type
                                                                              =>
                                                                              fun EA :
                                                                                CFML.SepLifted.Enc A
                                                                              =>
                                                                              fun Q :
                                                                                A ->
                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                              =>
                                                                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                fun p0__ :
                                                                                  Coq.Init.Datatypes.bool
                                                                                =>
                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                    Coq.Init.Logic.eq x2__ p0__
                                                                                  )
                                                                                ) (
                                                                                  @CFML.WPLifted.Wptag (
                                                                                    (
                                                                                      CFML.WPLifted.Wpgen_val k
                                                                                    )
                                                                                  ) _ _ Q
                                                                                )
                                                                              )
                                                                            ) (
                                                                              CFML.WPLifted.Wpgen_negpat (
                                                                                forall p0__ :
                                                                                  Coq.Init.Datatypes.bool,
                                                                                Coq.Init.Logic.not (
                                                                                  Coq.Init.Logic.eq x2__ p0__
                                                                                )
                                                                              )
                                                                            ) (
                                                                              @CFML.WPLifted.Wptag (
                                                                                @CFML.WPLifted.Wpgen_done
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        )
                                                      )
                                                    )
                                                  )
                                                ) _ _ Q
                                              )
                                            ) (
                                              CFML.WPLifted.Wpgen_negpat (
                                                Coq.Init.Logic.not (
                                                  Coq.Init.Logic.eq parent (
                                                    Empty : contents_
                                                  )
                                                )
                                              )
                                            ) (
                                              @CFML.WPLifted.Wptag (
                                                (
                                                  CFML.WPLifted.Wpgen_case (
                                                    fun A : Type =>
                                                    fun EA :
                                                      CFML.SepLifted.Enc A
                                                    =>
                                                    fun Q :
                                                      A ->
                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                    =>
                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                      fun parent_node : node_ =>
                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                          Coq.Init.Logic.eq parent (
                                                            Nonempty parent_node : contents_
                                                          )
                                                        )
                                                      ) (
                                                        @CFML.WPLifted.Wptag (
                                                          (
                                                            CFML.WPLifted.Wpgen_let_trm (
                                                              @CFML.WPLifted.Wptag (
                                                                (
                                                                  CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                    CFML.WPRecord.val_get_field value'
                                                                  ) (
                                                                    Coq.Lists.List.cons (
                                                                      @CFML.SepLifted.dyn_make node_ _ q
                                                                    ) Coq.Lists.List.nil
                                                                  )
                                                                )
                                                              )
                                                            ) (
                                                              fun x4__ :
                                                                Coq.ZArith.BinInt.Z
                                                              =>
                                                              @CFML.WPLifted.Wptag (
                                                                (
                                                                  CFML.WPLifted.Wpgen_let_trm (
                                                                    @CFML.WPLifted.Wptag (
                                                                      (
                                                                        CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                          CFML.WPRecord.val_get_field value'
                                                                        ) (
                                                                          Coq.Lists.List.cons (
                                                                            @CFML.SepLifted.dyn_make node_ _ parent_node
                                                                          ) Coq.Lists.List.nil
                                                                        )
                                                                      )
                                                                    )
                                                                  ) (
                                                                    fun x3__ :
                                                                      Coq.ZArith.BinInt.Z
                                                                    =>
                                                                    @CFML.WPLifted.Wptag (
                                                                      (
                                                                        CFML.WPLifted.Wpgen_if (
                                                                          (fun x__ y__ : Coq.ZArith.BinInt.Z => TLC.LibReflect.isTrue (@TLC.LibOrder.lt _ (@TLC.LibOrder.lt_of_le Coq.ZArith.BinInt.Z TLC.LibInt.le_int_inst) x__ y__)) x3__ (
                                                                            Coq.ZArith.BinInt.Z.sub x4__ d
                                                                          )
                                                                        ) (
                                                                          @CFML.WPLifted.Wptag (
                                                                            (
                                                                              CFML.WPLifted.Wpgen_let_trm (
                                                                                @CFML.WPLifted.Wptag (
                                                                                  (
                                                                                    CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                                      CFML.WPRecord.val_get_field value'
                                                                                    ) (
                                                                                      Coq.Lists.List.cons (
                                                                                        @CFML.SepLifted.dyn_make node_ _ q
                                                                                      ) Coq.Lists.List.nil
                                                                                    )
                                                                                  )
                                                                                )
                                                                              ) (
                                                                                fun x19__ :
                                                                                  Coq.ZArith.BinInt.Z
                                                                                =>
                                                                                @CFML.WPLifted.Wptag (
                                                                                  (
                                                                                    CFML.WPLifted.Wpgen_let_trm (
                                                                                      @CFML.WPLifted.Wptag (
                                                                                        (
                                                                                          CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                                            CFML.WPRecord.val_set_field value'
                                                                                          ) (
                                                                                            Coq.Lists.List.cons (
                                                                                              @CFML.SepLifted.dyn_make node_ _ q
                                                                                            ) (
                                                                                              Coq.Lists.List.cons (
                                                                                                @CFML.SepLifted.dyn_make Coq.ZArith.BinInt.Z _ (
                                                                                                  Coq.ZArith.BinInt.Z.sub x19__ d
                                                                                                )
                                                                                              ) Coq.Lists.List.nil
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    ) (
                                                                                      fun x20__ :
                                                                                        Coq.Init.Datatypes.unit
                                                                                      =>
                                                                                      @CFML.WPLifted.Wptag (
                                                                                        (
                                                                                          CFML.WPLifted.Wpgen_match x20__ (
                                                                                            @CFML.WPLifted.Wptag (
                                                                                              (
                                                                                                CFML.WPLifted.Wpgen_case (
                                                                                                  fun A :
                                                                                                    Type
                                                                                                  =>
                                                                                                  fun EA :
                                                                                                    CFML.SepLifted.Enc A
                                                                                                  =>
                                                                                                  fun Q :
                                                                                                    A ->
                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                  =>
                                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                    fun p0__ :
                                                                                                      Coq.Init.Datatypes.unit
                                                                                                    =>
                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                        Coq.Init.Logic.eq x20__ p0__
                                                                                                      )
                                                                                                    ) (
                                                                                                      @CFML.WPLifted.Wptag (
                                                                                                        (
                                                                                                          CFML.WPLifted.Wpgen_app contents_ root (
                                                                                                            Coq.Lists.List.cons (
                                                                                                              @CFML.SepLifted.dyn_make contents_ _ k
                                                                                                            ) Coq.Lists.List.nil
                                                                                                          )
                                                                                                        )
                                                                                                      ) _ _ Q
                                                                                                    )
                                                                                                  )
                                                                                                ) (
                                                                                                  CFML.WPLifted.Wpgen_negpat (
                                                                                                    forall p0__ :
                                                                                                      Coq.Init.Datatypes.unit,
                                                                                                    Coq.Init.Logic.not (
                                                                                                      Coq.Init.Logic.eq x20__ p0__
                                                                                                    )
                                                                                                  )
                                                                                                ) (
                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                    @CFML.WPLifted.Wpgen_done
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        ) (
                                                                          @CFML.WPLifted.Wptag (
                                                                            (
                                                                              CFML.WPLifted.Wpgen_let_trm (
                                                                                @CFML.WPLifted.Wptag (
                                                                                  (
                                                                                    CFML.WPLifted.Wpgen_app contents_ (
                                                                                      CFML.WPRecord.val_get_field child'
                                                                                    ) (
                                                                                      Coq.Lists.List.cons (
                                                                                        @CFML.SepLifted.dyn_make node_ _ parent_node
                                                                                      ) Coq.Lists.List.nil
                                                                                    )
                                                                                  )
                                                                                )
                                                                              ) (
                                                                                fun x5__ :
                                                                                  contents_
                                                                                =>
                                                                                @CFML.WPLifted.Wptag (
                                                                                  (
                                                                                    CFML.WPLifted.Wpgen_let_trm (
                                                                                      @CFML.WPLifted.Wptag (
                                                                                        (
                                                                                          CFML.WPLifted.Wpgen_match x5__ (
                                                                                            @CFML.WPLifted.Wptag (
                                                                                              (
                                                                                                CFML.WPLifted.Wpgen_case (
                                                                                                  fun A :
                                                                                                    Type
                                                                                                  =>
                                                                                                  fun EA :
                                                                                                    CFML.SepLifted.Enc A
                                                                                                  =>
                                                                                                  fun Q :
                                                                                                    A ->
                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                  =>
                                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                      Coq.Init.Logic.eq x5__ (
                                                                                                        Empty : contents_
                                                                                                      )
                                                                                                    )
                                                                                                  ) (
                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                      @CFML.WPLifted.Wpgen_fail
                                                                                                    ) _ _ Q
                                                                                                  )
                                                                                                ) (
                                                                                                  CFML.WPLifted.Wpgen_negpat (
                                                                                                    Coq.Init.Logic.not (
                                                                                                      Coq.Init.Logic.eq x5__ (
                                                                                                        Empty : contents_
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                ) (
                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                    (
                                                                                                      CFML.WPLifted.Wpgen_case (
                                                                                                        fun A :
                                                                                                          Type
                                                                                                        =>
                                                                                                        fun EA :
                                                                                                          CFML.SepLifted.Enc A
                                                                                                        =>
                                                                                                        fun Q :
                                                                                                          A ->
                                                                                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                        =>
                                                                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                          fun child :
                                                                                                            node_
                                                                                                          =>
                                                                                                          CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                              Coq.Init.Logic.eq x5__ (
                                                                                                                Nonempty child : contents_
                                                                                                              )
                                                                                                            )
                                                                                                          ) (
                                                                                                            @CFML.WPLifted.Wptag (
                                                                                                              (
                                                                                                                CFML.WPLifted.Wpgen_let_trm (
                                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                                    (
                                                                                                                      CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                                                                        CFML.WPRecord.val_get_field value'
                                                                                                                      ) (
                                                                                                                        Coq.Lists.List.cons (
                                                                                                                          @CFML.SepLifted.dyn_make node_ _ q
                                                                                                                        ) Coq.Lists.List.nil
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                ) (
                                                                                                                  fun x7__ :
                                                                                                                    Coq.ZArith.BinInt.Z
                                                                                                                  =>
                                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                                    (
                                                                                                                      CFML.WPLifted.Wpgen_let_trm (
                                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                                          (
                                                                                                                            CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                                                                              CFML.WPRecord.val_get_field value'
                                                                                                                            ) (
                                                                                                                              Coq.Lists.List.cons (
                                                                                                                                @CFML.SepLifted.dyn_make node_ _ child
                                                                                                                              ) Coq.Lists.List.nil
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      ) (
                                                                                                                        fun x6__ :
                                                                                                                          Coq.ZArith.BinInt.Z
                                                                                                                        =>
                                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                                          (
                                                                                                                            CFML.WPLifted.Wpgen_val (
                                                                                                                              (fun x__ y__ : Coq.ZArith.BinInt.Z => TLC.LibReflect.isTrue (Coq.Init.Logic.eq x__ y__)) x6__ x7__
                                                                                                                            )
                                                                                                                          )
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            ) _ _ Q
                                                                                                          )
                                                                                                        )
                                                                                                      ) (
                                                                                                        CFML.WPLifted.Wpgen_negpat (
                                                                                                          forall child :
                                                                                                            node_,
                                                                                                          Coq.Init.Logic.not (
                                                                                                            Coq.Init.Logic.eq x5__ (
                                                                                                              Nonempty child : contents_
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      ) (
                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                          @CFML.WPLifted.Wpgen_done
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    ) (
                                                                                      fun x8__ :
                                                                                        Coq.Init.Datatypes.bool
                                                                                      =>
                                                                                      @CFML.WPLifted.Wptag (
                                                                                        (
                                                                                          CFML.WPLifted.Wpgen_if x8__ (
                                                                                            @CFML.WPLifted.Wptag (
                                                                                              (
                                                                                                CFML.WPLifted.Wpgen_let_trm (
                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                    (
                                                                                                      CFML.WPLifted.Wpgen_app contents_ (
                                                                                                        CFML.WPRecord.val_get_field sibling'
                                                                                                      ) (
                                                                                                        Coq.Lists.List.cons (
                                                                                                          @CFML.SepLifted.dyn_make node_ _ q
                                                                                                        ) Coq.Lists.List.nil
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                ) (
                                                                                                  fun x14__ :
                                                                                                    contents_
                                                                                                  =>
                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                    (
                                                                                                      CFML.WPLifted.Wpgen_let_trm (
                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                          (
                                                                                                            CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                                                              CFML.WPRecord.val_set_field child'
                                                                                                            ) (
                                                                                                              Coq.Lists.List.cons (
                                                                                                                @CFML.SepLifted.dyn_make node_ _ parent_node
                                                                                                              ) (
                                                                                                                Coq.Lists.List.cons (
                                                                                                                  @CFML.SepLifted.dyn_make contents_ _ x14__
                                                                                                                ) Coq.Lists.List.nil
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      ) (
                                                                                                        fun x15__ :
                                                                                                          Coq.Init.Datatypes.unit
                                                                                                        =>
                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                          (
                                                                                                            CFML.WPLifted.Wpgen_match x15__ (
                                                                                                              @CFML.WPLifted.Wptag (
                                                                                                                (
                                                                                                                  CFML.WPLifted.Wpgen_case (
                                                                                                                    fun A :
                                                                                                                      Type
                                                                                                                    =>
                                                                                                                    fun EA :
                                                                                                                      CFML.SepLifted.Enc A
                                                                                                                    =>
                                                                                                                    fun Q :
                                                                                                                      A ->
                                                                                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                                    =>
                                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                                      fun p0__ :
                                                                                                                        Coq.Init.Datatypes.unit
                                                                                                                      =>
                                                                                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                                          Coq.Init.Logic.eq x15__ p0__
                                                                                                                        )
                                                                                                                      ) (
                                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                                          (
                                                                                                                            CFML.WPLifted.Wpgen_let_trm (
                                                                                                                              @CFML.WPLifted.Wptag (
                                                                                                                                (
                                                                                                                                  CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                                                                                    CFML.WPRecord.val_get_field value'
                                                                                                                                  ) (
                                                                                                                                    Coq.Lists.List.cons (
                                                                                                                                      @CFML.SepLifted.dyn_make node_ _ q
                                                                                                                                    ) Coq.Lists.List.nil
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            ) (
                                                                                                                              fun x16__ :
                                                                                                                                Coq.ZArith.BinInt.Z
                                                                                                                              =>
                                                                                                                              @CFML.WPLifted.Wptag (
                                                                                                                                (
                                                                                                                                  CFML.WPLifted.Wpgen_let_trm (
                                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                                      (
                                                                                                                                        CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                                                                                          CFML.WPRecord.val_set_field value'
                                                                                                                                        ) (
                                                                                                                                          Coq.Lists.List.cons (
                                                                                                                                            @CFML.SepLifted.dyn_make node_ _ q
                                                                                                                                          ) (
                                                                                                                                            Coq.Lists.List.cons (
                                                                                                                                              @CFML.SepLifted.dyn_make Coq.ZArith.BinInt.Z _ (
                                                                                                                                                Coq.ZArith.BinInt.Z.sub x16__ d
                                                                                                                                              )
                                                                                                                                            ) Coq.Lists.List.nil
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  ) (
                                                                                                                                    fun x17__ :
                                                                                                                                      Coq.Init.Datatypes.unit
                                                                                                                                    =>
                                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                                      (
                                                                                                                                        CFML.WPLifted.Wpgen_match x17__ (
                                                                                                                                          @CFML.WPLifted.Wptag (
                                                                                                                                            (
                                                                                                                                              CFML.WPLifted.Wpgen_case (
                                                                                                                                                fun A :
                                                                                                                                                  Type
                                                                                                                                                =>
                                                                                                                                                fun EA :
                                                                                                                                                  CFML.SepLifted.Enc A
                                                                                                                                                =>
                                                                                                                                                fun Q :
                                                                                                                                                  A ->
                                                                                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                                                                =>
                                                                                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                                                                  fun p0__ :
                                                                                                                                                    Coq.Init.Datatypes.unit
                                                                                                                                                  =>
                                                                                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                                                                      Coq.Init.Logic.eq x17__ p0__
                                                                                                                                                    )
                                                                                                                                                  ) (
                                                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                                                      (
                                                                                                                                                        CFML.WPLifted.Wpgen_let_trm (
                                                                                                                                                          @CFML.WPLifted.Wptag (
                                                                                                                                                            (
                                                                                                                                                              CFML.WPLifted.Wpgen_app contents_ root (
                                                                                                                                                                Coq.Lists.List.cons (
                                                                                                                                                                  @CFML.SepLifted.dyn_make contents_ _ k
                                                                                                                                                                ) Coq.Lists.List.nil
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        ) (
                                                                                                                                                          fun x18__ :
                                                                                                                                                            contents_
                                                                                                                                                          =>
                                                                                                                                                          @CFML.WPLifted.Wptag (
                                                                                                                                                            (
                                                                                                                                                              CFML.WPLifted.Wpgen_app contents_ merge (
                                                                                                                                                                Coq.Lists.List.cons (
                                                                                                                                                                  @CFML.SepLifted.dyn_make contents_ _ x18__
                                                                                                                                                                ) (
                                                                                                                                                                  Coq.Lists.List.cons (
                                                                                                                                                                    @CFML.SepLifted.dyn_make contents_ _ k
                                                                                                                                                                  ) Coq.Lists.List.nil
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    ) _ _ Q
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ) (
                                                                                                                                                CFML.WPLifted.Wpgen_negpat (
                                                                                                                                                  forall p0__ :
                                                                                                                                                    Coq.Init.Datatypes.unit,
                                                                                                                                                  Coq.Init.Logic.not (
                                                                                                                                                    Coq.Init.Logic.eq x17__ p0__
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ) (
                                                                                                                                                @CFML.WPLifted.Wptag (
                                                                                                                                                  @CFML.WPLifted.Wpgen_done
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        ) _ _ Q
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ) (
                                                                                                                    CFML.WPLifted.Wpgen_negpat (
                                                                                                                      forall p0__ :
                                                                                                                        Coq.Init.Datatypes.unit,
                                                                                                                      Coq.Init.Logic.not (
                                                                                                                        Coq.Init.Logic.eq x15__ p0__
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ) (
                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                      @CFML.WPLifted.Wpgen_done
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          ) (
                                                                                            @CFML.WPLifted.Wptag (
                                                                                              (
                                                                                                CFML.WPLifted.Wpgen_let_trm (
                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                    (
                                                                                                      CFML.WPLifted.Wpgen_app contents_ left_sibling (
                                                                                                        Coq.Lists.List.cons (
                                                                                                          @CFML.SepLifted.dyn_make contents_ _ k
                                                                                                        ) Coq.Lists.List.nil
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                ) (
                                                                                                  fun ls :
                                                                                                    contents_
                                                                                                  =>
                                                                                                  @CFML.WPLifted.Wptag (
                                                                                                    (
                                                                                                      CFML.WPLifted.Wpgen_match ls (
                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                          (
                                                                                                            CFML.WPLifted.Wpgen_case (
                                                                                                              fun A :
                                                                                                                Type
                                                                                                              =>
                                                                                                              fun EA :
                                                                                                                CFML.SepLifted.Enc A
                                                                                                              =>
                                                                                                              fun Q :
                                                                                                                A ->
                                                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                              =>
                                                                                                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                                  Coq.Init.Logic.eq ls (
                                                                                                                    Empty : contents_
                                                                                                                  )
                                                                                                                )
                                                                                                              ) (
                                                                                                                @CFML.WPLifted.Wptag (
                                                                                                                  @CFML.WPLifted.Wpgen_fail
                                                                                                                ) _ _ Q
                                                                                                              )
                                                                                                            ) (
                                                                                                              CFML.WPLifted.Wpgen_negpat (
                                                                                                                Coq.Init.Logic.not (
                                                                                                                  Coq.Init.Logic.eq ls (
                                                                                                                    Empty : contents_
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            ) (
                                                                                                              @CFML.WPLifted.Wptag (
                                                                                                                (
                                                                                                                  CFML.WPLifted.Wpgen_case (
                                                                                                                    fun A :
                                                                                                                      Type
                                                                                                                    =>
                                                                                                                    fun EA :
                                                                                                                      CFML.SepLifted.Enc A
                                                                                                                    =>
                                                                                                                    fun Q :
                                                                                                                      A ->
                                                                                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                                    =>
                                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                                      fun lsq :
                                                                                                                        node_
                                                                                                                      =>
                                                                                                                      CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                                        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                                          Coq.Init.Logic.eq ls (
                                                                                                                            Nonempty lsq : contents_
                                                                                                                          )
                                                                                                                        )
                                                                                                                      ) (
                                                                                                                        @CFML.WPLifted.Wptag (
                                                                                                                          (
                                                                                                                            CFML.WPLifted.Wpgen_let_trm (
                                                                                                                              @CFML.WPLifted.Wptag (
                                                                                                                                (
                                                                                                                                  CFML.WPLifted.Wpgen_app contents_ (
                                                                                                                                    CFML.WPRecord.val_get_field sibling'
                                                                                                                                  ) (
                                                                                                                                    Coq.Lists.List.cons (
                                                                                                                                      @CFML.SepLifted.dyn_make node_ _ q
                                                                                                                                    ) Coq.Lists.List.nil
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            ) (
                                                                                                                              fun x9__ :
                                                                                                                                contents_
                                                                                                                              =>
                                                                                                                              @CFML.WPLifted.Wptag (
                                                                                                                                (
                                                                                                                                  CFML.WPLifted.Wpgen_let_trm (
                                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                                      (
                                                                                                                                        CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                                                                                          CFML.WPRecord.val_set_field sibling'
                                                                                                                                        ) (
                                                                                                                                          Coq.Lists.List.cons (
                                                                                                                                            @CFML.SepLifted.dyn_make node_ _ lsq
                                                                                                                                          ) (
                                                                                                                                            Coq.Lists.List.cons (
                                                                                                                                              @CFML.SepLifted.dyn_make contents_ _ x9__
                                                                                                                                            ) Coq.Lists.List.nil
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  ) (
                                                                                                                                    fun x10__ :
                                                                                                                                      Coq.Init.Datatypes.unit
                                                                                                                                    =>
                                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                                      (
                                                                                                                                        CFML.WPLifted.Wpgen_match x10__ (
                                                                                                                                          @CFML.WPLifted.Wptag (
                                                                                                                                            (
                                                                                                                                              CFML.WPLifted.Wpgen_case (
                                                                                                                                                fun A :
                                                                                                                                                  Type
                                                                                                                                                =>
                                                                                                                                                fun EA :
                                                                                                                                                  CFML.SepLifted.Enc A
                                                                                                                                                =>
                                                                                                                                                fun Q :
                                                                                                                                                  A ->
                                                                                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                                                                =>
                                                                                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                                                                  fun p0__ :
                                                                                                                                                    Coq.Init.Datatypes.unit
                                                                                                                                                  =>
                                                                                                                                                  CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                                                                    CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                                                                      Coq.Init.Logic.eq x10__ p0__
                                                                                                                                                    )
                                                                                                                                                  ) (
                                                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                                                      (
                                                                                                                                                        CFML.WPLifted.Wpgen_let_trm (
                                                                                                                                                          @CFML.WPLifted.Wptag (
                                                                                                                                                            (
                                                                                                                                                              CFML.WPLifted.Wpgen_app Coq.ZArith.BinInt.Z (
                                                                                                                                                                CFML.WPRecord.val_get_field value'
                                                                                                                                                              ) (
                                                                                                                                                                Coq.Lists.List.cons (
                                                                                                                                                                  @CFML.SepLifted.dyn_make node_ _ q
                                                                                                                                                                ) Coq.Lists.List.nil
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        ) (
                                                                                                                                                          fun x11__ :
                                                                                                                                                            Coq.ZArith.BinInt.Z
                                                                                                                                                          =>
                                                                                                                                                          @CFML.WPLifted.Wptag (
                                                                                                                                                            (
                                                                                                                                                              CFML.WPLifted.Wpgen_let_trm (
                                                                                                                                                                @CFML.WPLifted.Wptag (
                                                                                                                                                                  (
                                                                                                                                                                    CFML.WPLifted.Wpgen_app Coq.Init.Datatypes.unit (
                                                                                                                                                                      CFML.WPRecord.val_set_field value'
                                                                                                                                                                    ) (
                                                                                                                                                                      Coq.Lists.List.cons (
                                                                                                                                                                        @CFML.SepLifted.dyn_make node_ _ q
                                                                                                                                                                      ) (
                                                                                                                                                                        Coq.Lists.List.cons (
                                                                                                                                                                          @CFML.SepLifted.dyn_make Coq.ZArith.BinInt.Z _ (
                                                                                                                                                                            Coq.ZArith.BinInt.Z.sub x11__ d
                                                                                                                                                                          )
                                                                                                                                                                        ) Coq.Lists.List.nil
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              ) (
                                                                                                                                                                fun x12__ :
                                                                                                                                                                  Coq.Init.Datatypes.unit
                                                                                                                                                                =>
                                                                                                                                                                @CFML.WPLifted.Wptag (
                                                                                                                                                                  (
                                                                                                                                                                    CFML.WPLifted.Wpgen_match x12__ (
                                                                                                                                                                      @CFML.WPLifted.Wptag (
                                                                                                                                                                        (
                                                                                                                                                                          CFML.WPLifted.Wpgen_case (
                                                                                                                                                                            fun A :
                                                                                                                                                                              Type
                                                                                                                                                                            =>
                                                                                                                                                                            fun EA :
                                                                                                                                                                              CFML.SepLifted.Enc A
                                                                                                                                                                            =>
                                                                                                                                                                            fun Q :
                                                                                                                                                                              A ->
                                                                                                                                                                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hprop
                                                                                                                                                                            =>
                                                                                                                                                                            CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hforall (
                                                                                                                                                                              fun p0__ :
                                                                                                                                                                                Coq.Init.Datatypes.unit
                                                                                                                                                                              =>
                                                                                                                                                                              CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hwand (
                                                                                                                                                                                CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hpure (
                                                                                                                                                                                  Coq.Init.Logic.eq x12__ p0__
                                                                                                                                                                                )
                                                                                                                                                                              ) (
                                                                                                                                                                                @CFML.WPLifted.Wptag (
                                                                                                                                                                                  (
                                                                                                                                                                                    CFML.WPLifted.Wpgen_let_trm (
                                                                                                                                                                                      @CFML.WPLifted.Wptag (
                                                                                                                                                                                        (
                                                                                                                                                                                          CFML.WPLifted.Wpgen_app contents_ root (
                                                                                                                                                                                            Coq.Lists.List.cons (
                                                                                                                                                                                              @CFML.SepLifted.dyn_make contents_ _ k
                                                                                                                                                                                            ) Coq.Lists.List.nil
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    ) (
                                                                                                                                                                                      fun x13__ :
                                                                                                                                                                                        contents_
                                                                                                                                                                                      =>
                                                                                                                                                                                      @CFML.WPLifted.Wptag (
                                                                                                                                                                                        (
                                                                                                                                                                                          CFML.WPLifted.Wpgen_app contents_ merge (
                                                                                                                                                                                            Coq.Lists.List.cons (
                                                                                                                                                                                              @CFML.SepLifted.dyn_make contents_ _ x13__
                                                                                                                                                                                            ) (
                                                                                                                                                                                              Coq.Lists.List.cons (
                                                                                                                                                                                                @CFML.SepLifted.dyn_make contents_ _ k
                                                                                                                                                                                              ) Coq.Lists.List.nil
                                                                                                                                                                                            )
                                                                                                                                                                                          )
                                                                                                                                                                                        )
                                                                                                                                                                                      )
                                                                                                                                                                                    )
                                                                                                                                                                                  )
                                                                                                                                                                                ) _ _ Q
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          ) (
                                                                                                                                                                            CFML.WPLifted.Wpgen_negpat (
                                                                                                                                                                              forall p0__ :
                                                                                                                                                                                Coq.Init.Datatypes.unit,
                                                                                                                                                                              Coq.Init.Logic.not (
                                                                                                                                                                                Coq.Init.Logic.eq x12__ p0__
                                                                                                                                                                              )
                                                                                                                                                                            )
                                                                                                                                                                          ) (
                                                                                                                                                                            @CFML.WPLifted.Wptag (
                                                                                                                                                                              @CFML.WPLifted.Wpgen_done
                                                                                                                                                                            )
                                                                                                                                                                          )
                                                                                                                                                                        )
                                                                                                                                                                      )
                                                                                                                                                                    )
                                                                                                                                                                  )
                                                                                                                                                                )
                                                                                                                                                              )
                                                                                                                                                            )
                                                                                                                                                          )
                                                                                                                                                        )
                                                                                                                                                      )
                                                                                                                                                    ) _ _ Q
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ) (
                                                                                                                                                CFML.WPLifted.Wpgen_negpat (
                                                                                                                                                  forall p0__ :
                                                                                                                                                    Coq.Init.Datatypes.unit,
                                                                                                                                                  Coq.Init.Logic.not (
                                                                                                                                                    Coq.Init.Logic.eq x10__ p0__
                                                                                                                                                  )
                                                                                                                                                )
                                                                                                                                              ) (
                                                                                                                                                @CFML.WPLifted.Wptag (
                                                                                                                                                  @CFML.WPLifted.Wpgen_done
                                                                                                                                                )
                                                                                                                                              )
                                                                                                                                            )
                                                                                                                                          )
                                                                                                                                        )
                                                                                                                                      )
                                                                                                                                    )
                                                                                                                                  )
                                                                                                                                )
                                                                                                                              )
                                                                                                                            )
                                                                                                                          )
                                                                                                                        ) _ _ Q
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ) (
                                                                                                                    CFML.WPLifted.Wpgen_negpat (
                                                                                                                      forall lsq :
                                                                                                                        node_,
                                                                                                                      Coq.Init.Logic.not (
                                                                                                                        Coq.Init.Logic.eq ls (
                                                                                                                          Nonempty lsq : contents_
                                                                                                                        )
                                                                                                                      )
                                                                                                                    )
                                                                                                                  ) (
                                                                                                                    @CFML.WPLifted.Wptag (
                                                                                                                      @CFML.WPLifted.Wpgen_done
                                                                                                                    )
                                                                                                                  )
                                                                                                                )
                                                                                                              )
                                                                                                            )
                                                                                                          )
                                                                                                        )
                                                                                                      )
                                                                                                    )
                                                                                                  )
                                                                                                )
                                                                                              )
                                                                                            )
                                                                                          )
                                                                                        )
                                                                                      )
                                                                                    )
                                                                                  )
                                                                                )
                                                                              )
                                                                            )
                                                                          )
                                                                        )
                                                                      )
                                                                    )
                                                                  )
                                                                )
                                                              )
                                                            )
                                                          )
                                                        ) _ _ Q
                                                      )
                                                    )
                                                  ) (
                                                    CFML.WPLifted.Wpgen_negpat (
                                                      forall parent_node :
                                                        node_,
                                                      Coq.Init.Logic.not (
                                                        Coq.Init.Logic.eq parent (
                                                          Nonempty parent_node : contents_
                                                        )
                                                      )
                                                    )
                                                  ) (
                                                    @CFML.WPLifted.Wptag (
                                                      @CFML.WPLifted.Wpgen_done
                                                    )
                                                  )
                                                )
                                              )
                                            )
                                          )
                                        )
                                      )
                                    )
                                  )
                                )
                              )
                            ) _ _ Q
                          )
                        )
                      ) (
                        CFML.WPLifted.Wpgen_negpat (
                          forall q : node_,
                          Coq.Init.Logic.not (
                            Coq.Init.Logic.eq k (Nonempty q : contents_)
                          )
                        )
                      ) (@CFML.WPLifted.Wptag (@CFML.WPLifted.Wpgen_done))
                    )
                  )
                )
              )
            )
          )
        )
      ) _ _ (
        fun res__ : _ =>
        CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hstar (Q res__) CFML.SepBase.SepBasicSetup.SepSimplArgsCredits.hgc
      )
    ) ->
    CFML.SepLifted.Triple (
      CFML.SepLifted.Trm_apps decrease_key (
        Coq.Lists.List.cons (@CFML.SepLifted.dyn_make contents_ _ k) (
          Coq.Lists.List.cons (@CFML.SepLifted.dyn_make Coq.ZArith.BinInt.Z _ d) Coq.Lists.List.nil
        )
      )
    ) H Q
  ).

Hint Extern 1 (WPHeader_Register_CF decrease_key) => WPHeader_Provide decrease_key_cf__.

