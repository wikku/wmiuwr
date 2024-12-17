(** * Rules: Reasoning Rules for Term Constructs *)

Set Implicit Arguments.

From SLF Require Export LibSepReference.
From SLF Require Basic.

Implicit Types n : int.
Implicit Types t : trm.
Implicit Types r v : val.
Implicit Types p : loc.
Implicit Types H : hprop.
Implicit Types Q : val->hprop.

(* ################################################################# *)
(** * First Pass *)

(** The present chapter establishes the remaining reasoning rules of Separation
    Logic: rules for reasoning about each language construct, and rules for
    reasoning about primitive operations. All these reasoning rules are
    expressed using the predicate [triple], and are proved with respect to the
    formal semantics captured by the predicate [eval] introduced in the previous
    chapter ([Triples]). Throughout the chapter, one needs to have in mind
    the definition of [triple]:

    Definition triple (t:trm) (H:hprop) (Q:val->hprop) : Prop :=
      forall s, H s -> eval s t Q.

    After establishing the reasoning rules, the present chapter presents an
    example proof of a concrete program, using directly these reasoning rules,
    as opposed to using high-level "x-tactics" like done in the first two
    chapters. This example proof reveals the proof patterns employed by the
    x-tactics. It also motivates the introduction of the x-tactics to avoid the
    tedious, repetitive proof patterns. *)

(* ================================================================= *)
(** ** Rules for Term Constructs *)

(* ----------------------------------------------------------------- *)
(** *** Reasoning Rule for Sequences *)

(** Let us begin with the reasoning rule for sequences. The Separation Logic
    reasoning rule for a sequence [t1;t2] is essentially the same as that from
    Hoare logic. The rule is:

      {H} t1 {fun v => H1}     {H1} t2 {Q}
      ------------------------------------
              {H} (t1;t2) {Q}

    The Coq statement corresponding to the above rule is: *)

Lemma triple_seq : forall t1 t2 H Q H1,
  triple t1 H (fun v => H1) ->
  triple t2 H1 Q ->
  triple (trm_seq t1 t2) H Q.

(** The variable [v] denotes the result of the evaluation of [t1]. It is
    unconstrained in the rule. For well-typed programs, this result is
    necessarily the unit value. For untyped programs, the result of [t1] is
    simply irrelevant. *)

(** **** Exercise: 2 stars, standard, especially useful (triple_seq)

    Prove [triple_seq] by unfolding [triple] and using [eval_seq]. *)

Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(* ----------------------------------------------------------------- *)
(** *** Reasoning Rule for Let-Bindings *)

(** Next, we present the reasoning rule for let-bindings. Here again, there is
    nothing specific to Separation Logic. The rule is exactly the same as in
    Hoare Logic. The reasoning rule for a let binding [let x = t1 in t2] could
    be stated, in informal writing, in the form:

      {H} t1 {Q1}     (forall x, {Q1 x} t2 {Q})
      -----------------------------------------
            {H} (let x = t1 in t2) {Q}

  Yet, such a presentation makes a confusion between the [x] that denotes a
  program variable in [let x = t1 in t2], and the [x] that denotes a universally
  quantified Coq. The correct statement involves a substitution from the
  variable [x] to a value quantified as [forall (v:val)].

      {H} t1 {Q1}     (forall v, {Q1 v} (subst x v t2) {Q})
      -----------------------------------------------------
                {H} (let x = t1 in t2) {Q}

  The corresponding Coq statement is thus as follows. *)

Lemma triple_let : forall x t1 t2 Q1 H Q,
  triple t1 H Q1 ->
  (forall v1, triple (subst x v1 t2) (Q1 v1) Q) ->
  triple (trm_let x t1 t2) H Q.
Proof using. introv M1 M2 Hs. applys* eval_let. Qed.


(* ----------------------------------------------------------------- *)
(** *** Reasoning Rule for Conditionals *)

(** The rule for a conditional is, again, exactly like in Hoare logic.

      b = true -> {H} t1 {Q}     b = false -> {H} t2 {Q}
      --------------------------------------------------
               {H} (if b then t1 in t2) {Q}

  The corresponding Coq statement appears next. *)

Lemma triple_if_case : forall b t1 t2 H Q,
  (b = true -> triple t1 H Q) ->
  (b = false -> triple t2 H Q) ->
  triple (trm_if (val_bool b) t1 t2) H Q.

(** **** Exercise: 2 stars, standard, especially useful (triple_if_case)

    Prove [triple_if_case] by unfolding [triple] and using [eval_if]. Hint: use
    the tactic [case_if] to perform a case analysis. *)

Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** The two premises of the rule for if-statements may be factorized into a
    single one using Coq's conditional construct, like in [eval_if]. *)

Lemma triple_if : forall (b:bool) t1 t2 H Q,
  triple (if b then t1 else t2) H Q ->
  triple (trm_if b t1 t2) H Q.
Proof using. introv M Hs. applys* eval_if. Qed.

(* ----------------------------------------------------------------- *)
(** *** Reasoning Rule for Values *)

(** The triple specifying the behavior of a value [v] can be written as a triple
    with an empty precondition and a postcondition asserting that the result
    value [r] is equal to [v], in the empty heap. Formally:

     ----------------------------
      {\[]} v {fun r => \[r = v]}

    In practice, however, it is more convenient in practice to work with a
    judgment whose conclusion is of the form [{H} v {Q}], for an arbitrary
    precondition [H] and an arbitrary postcondition [Q].

      H ==> Q v
      ---------
      {H} v {Q}

    The Coq statement of the rule for values is thus as follows. *)

Lemma triple_val : forall v H Q,
  H ==> Q v ->
  triple (trm_val v) H Q.
Proof using. introv M Hs. applys* eval_val. Qed.

(** It may not be completely obvious at first sight why [triple_val] is
    equivalent to the "minimalistic" rule [{\[]} v {fun r => \[r = v]}]. Let us
    prove the equivalence. *)

(** **** Exercise: 1 star, standard, especially useful (triple_val_minimal)

    Prove that the alternative rule for values derivable from [triple_val].
    Hint: use the tactic [xsimpl] to conclude the proof. *)

Lemma triple_val_minimal : forall v,
  triple (trm_val v) \[] (fun r => \[r = v]).
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** **** Exercise: 2 stars, standard, especially useful (triple_val')

    More interestingly, prove that [triple_val] is derivable from
    [triple_val_minimal]. Concretely, your goal is to prove [triple_val']
    without unfolding the definition of [triple]. Use [xsimpl],
    [triple_val_minimal] and the structural rules on [triple] such as
    [triple_conseq_frame]. *)

Lemma triple_val' : forall v H Q,
  H ==> Q v ->
  triple (trm_val v) H Q.
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** **** Exercise: 4 stars, standard, especially useful (triple_let_val)

    Consider a term of the form [let x = v1 in t2], that is, where the argument
    of the let-binding is already a value. State and prove a reasoning rule of
    the form:

      Lemma triple_let_val : forall x v1 t2 H Q,
        ... ->
        triple (trm_let x v1 t2) H Q.
*)

(* FILL IN HERE *)

(** [] *)

(* ----------------------------------------------------------------- *)
(** *** Reasoning Rule for Function Definitions *)

(** In addition to the reasoning rule for values, we need reasoning rules for
    functions and recursive functions that appear as terms in the source program
    (as opposed to appearing as values).

    A function definition [trm_fun x t1], expressed as a subterm in a program,
    evaluates to a value, more precisely to [val_fun x t1]. Again, we could
    consider a rule with an empty precondition:

     ------------------------------------------------------
      {\[]} (trm_fun x t1) {fun r => \[r = val_fun x t1]}

   However, it is easier to apply a rule featuring a conclusion of the form
   [{H} (trm_fun x t1) {Q}]. We thus consider the following rule, very similar
   to [triple_val]. *)

Lemma triple_fun : forall x t1 H Q,
  H ==> Q (val_fun x t1) ->
  triple (trm_fun x t1) H Q.
Proof using. introv M Hs. applys* eval_fun. Qed.

(** This reasoning rules for functions generalizes to recursive functions. A
    term describing a recursive function is written [trm_fix f x t1], and the
    corresponding value is written [val_fix f x t1]. *)

Lemma triple_fix : forall f x t1 H Q,
  H ==> Q (val_fix f x t1) ->
  triple (trm_fix f x t1) H Q.
Proof using. introv M Hs. applys* eval_fix. Qed.

(* ----------------------------------------------------------------- *)
(** *** Reasoning Rule for Function Calls *)

(** Last but not least, we need a reasoning rule to reason about a function
    application. Consider an application [trm_app v1 v2]. Assume [v1] to be a
    function, that is, to be of the form [val_fun x t1]. Then, according to the
    beta-reduction rule, the semantics of [trm_app v1 v2] is the same as that of
    [subst x v2 t1]. This reasoning rule is thus:

     v1 = val_fun x t1     {H} (subst x v2 t1) {Q}
     ---------------------------------------------
      {H} (trm_app v1 v2) {Q}

   The corresponding Coq statement is as shown below. *)

Lemma triple_app_fun : forall x v1 v2 t1 H Q,
  v1 = val_fun x t1 ->
  triple (subst x v2 t1) H Q ->
  triple (trm_app v1 v2) H Q.
Proof using. introv E M Hs. applys* eval_app_fun. Qed.

(** The reasoning rule that corresponds to beta-reduction for a recursive
    function involves two substitutions: a first substitution for recursive
    occurrences of the function, followed with a second substitution for the
    argument provided to the call. *)

Lemma triple_app_fix : forall v1 v2 f x t1 H Q,
  v1 = val_fix f x t1 ->
  triple (subst x v2 (subst f v1 t1)) H Q ->
  triple (trm_app v1 v2) H Q.
Proof using. introv E M Hs. applys* eval_app_fix. Qed.

(** This concludes the presentation of the reasoning rules for term constructs.
    Before we can tackle the verification of actual programs, there remains to
    present the specifications for the primitive operations. We start with
    arithmetic operations, then consider heap-manipulating operations. *)

(* ================================================================= *)
(** ** Rules for Primitive Arithmetic Operations *)

(* ----------------------------------------------------------------- *)
(** *** Addition *)

(** Consider a term of the form [val_add n1 n2], which is short for
    [trm_app (trm_app (trm_val val_add) (val_int n1)) (val_int n2)], if we
    unfold the coercion [val_int].

    The addition operation may execute in an empty state. It does not modify the
    state, and returns the value [val_int (n1+n2)]. Recall the statement of the
    evaluation rule, in omni-big-step style. *)

Parameter eval_add : forall s n1 n2 Q,
  Q (val_int (n1 + n2)) s ->
  eval s (val_add (val_int n1) (val_int n2)) Q.

(** In the specification shown below, the precondition is written [\[]] and the
    postcondition binds a return value [r] of type [val] specified to be equal
    to [val_int (n1+n2)]. *)

Lemma triple_add : forall n1 n2,
  triple (val_add n1 n2)
    \[]
    (fun r => \[r = val_int (n1 + n2)]).
Proof using.
  introv Hs. applys* eval_add. lets ->: hempty_inv Hs.
  applys hpure_intro. auto.
Qed.

(* ----------------------------------------------------------------- *)
(** *** Division *)

(** The specification of the division operation [val_div n1 n2] is similar, yet
    with the extra requirement that the argument [n2] must be nonzero. This
    requirement [n2 <> 0] appears in the evaluation rule as well as in the
    reasoning rule. *)

Parameter eval_div : forall s n1 n2 Q,
  n2 <> 0 ->
  Q (val_int (Z.quot n1 n2)) s ->
  eval s (val_div (val_int n1) (val_int n2)) Q.

Lemma triple_div : forall n1 n2,
  n2 <> 0 ->
  triple (val_div n1 n2)
    \[]
    (fun r => \[r = val_int (Z.quot n1 n2)]).
Proof using.
  introv Hn2 Hs. applys* eval_div. lets ->: hempty_inv Hs.
  applys hpure_intro. auto.
Qed.

(* ----------------------------------------------------------------- *)
(** *** Random-Number Generation *)

(** The evaluation of [val_rand n] is particularly interesting because it
    involves nondeterminism. Assume [n > 0]. The output value may be any [n1] in
    the range [0 <= n1 < n]. The statement [eval s (val_rand n) Q] should
    capture the fact that [Q s n1] holds, for any possible output integer [n1].
    This condition is captured by [forall n1, 0 <= n1 < n -> Q n1 s]. *)

Parameter eval_rand : forall s n Q,
  n > 0 ->
  (forall n1, 0 <= n1 < n -> Q n1 s) ->
  eval s (val_rand (val_int n)) Q.

(** The postcondition of the triple for [val_rand n] asserts that the output
    value [r] corresponds to an integer [n1], in the range from [0 <= n1 < n].
    *)

(** **** Exercise: 2 stars, standard, especially useful (triple_rand)

    Prove the reasoning rule for calls to the random number generator. *)

Lemma triple_rand : forall n,
  n > 0 ->
  triple (val_rand n)
    \[]
    (fun r => \[exists n1, r = val_int n1 /\ 0 <= n1 < n]).
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(* ================================================================= *)
(** ** Rules for Primitive Heap-Manipulating Operations *)

(** Establishing triples for the operations [ref], [get], [set] and [free]
    require reasoning about the definition of certain primitive heap predicates.
    This reasoning is to be performed using the introduction and elimination
    lemmas introduced in chapter [Hprop]. For example, [hsingle_intro]
    proves [(p ~~> v) (Fmap.single p v)]. Besides, the lemma [hstar_hpure_l]
    establishing [(\[P] \* H) h = (P /\ H h)] will be used pervasively. *)

(* ----------------------------------------------------------------- *)
(** *** Allocation *)

(** Recall that [val_ref v] allocates a cell with contents [v]. A call to
    [val_ref v] does not depend on the current contents of the state. The
    operation extends the state with a fresh singleton cell, at some location
    [p], assigning it [v] as contents. This fresh cell is described by the heap
    predicate [p ~~> v]. The evaluation of [val_ref v] produces the value
    [val_loc p]. Thus, if [r] denotes the result value, we have [r = val_loc p]
    for some [p]. In the corresponding specification shown below, observe how
    the location [p] is existentially quantified in the postcondition. *)

Lemma triple_ref : forall v,
  triple (val_ref v)
    \[]
    (fun r => \exists p, \[r = val_loc p] \* p ~~> v).
Proof using.
  intros. intros s1 K. applys eval_ref. intros p D.
  lets ->: hempty_inv K. rewrite Fmap.update_empty.
  applys hexists_intro p. rewrite hstar_hpure_l. split*.
  applys hsingle_intro.
Qed.

(** Using the notation [funloc p => H] as a shorthand for
    [fun (r:val) => \exists (p:loc), \[r = val_loc p] \* H], the specification
    for [val_ref] becomes more concise. *)

Lemma triple_ref' : forall v,
  triple (val_ref v)
    \[]
    (funloc p => p ~~> v).
Proof using. apply triple_ref. Qed.

(* ----------------------------------------------------------------- *)
(** *** Deallocation *)

(** Recall that [val_free] denotes the operation for deallocating a cell at a
    given address. A call of the form [val_free p] executes safely in a state
    described by [p ~~> v]. The operation leaves an empty state, and asserts
    that the return value, named [r], is equal to unit. *)

(** **** Exercise: 2 stars, standard, especially useful (triple_free')

    Prove the reasoning rule for deallocation of a single cell. Hint: use
    [Fmap.indom_single] and [Fmap.remove_single]. *)

Lemma triple_free' : forall p v,
  triple (val_free (val_loc p))
    (p ~~> v)
    (fun r => \[r = val_unit]).
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** In practice, the information [r = val_unit] is generally useless. Thus, we
    state [triple_free] with an empty postcondition. *)

Lemma triple_free : forall p v,
  triple (val_free (val_loc p))
    (p ~~> v)
    (fun _ => \[]).
Proof using. intros. applys triple_conseq triple_free'; xsimpl*. Qed.

(* ----------------------------------------------------------------- *)
(** *** Read *)

(** Recall that [val_get] denotes the operation for reading a memory cell. A
    call of the form [val_get v'] executes safely if its argument [v'] is a
    value of the form [val_loc p] for some location [p], in a state that
    features a memory cell at location [p], storing some contents [v]. Such a
    state is described as [p ~~> v]. The read operation returns a value [r] such
    that [r = v], and the memory state of the operation remains unchanged. The
    specification of [val_get] is thus expressed as follows. *)

Lemma triple_get : forall v p,
  triple (val_get p)
    (p ~~> v)
    (fun r => \[r = v] \* (p ~~> v)).
Proof using.
  intros. intros s K. lets ->: hsingle_inv K. applys eval_get.
  { applys* Fmap.indom_single. }
  { rewrite hstar_hpure_l. split*. rewrite* Fmap.read_single. }
Qed.

(* ----------------------------------------------------------------- *)
(** *** Write *)

(** Recall that [val_set] denotes the operation for writing a memory cell. A
    call of the form [val_set v' w] executes safely if [v'] is of the form
    [val_loc p] for some location [p], in a state [p ~~> v]. The write operation
    updates this state to [p ~~> w], and returns the unit value, which can be
    ignored. Hence, [val_set] is specified as follows. *)

(** **** Exercise: 2 stars, standard, especially useful (triple_set)

    Prove the reasoning rule for a write operation. Hint: use
    [Fmap.update_single]. *)

Lemma triple_set : forall w p v,
  triple (val_set (val_loc p) v)
    (p ~~> w)
    (fun r => \[r = val_unit] \* (p ~~> v)).
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(* ================================================================= *)
(** ** Program Verification using the Reasoning Rules of Separation Logic *)

(** We have at hand all the necessary rules for carrying out actual verification
    proofs in Separation Logic. *)

Module ExamplePrograms.
Export ProgramSyntax.

(* ----------------------------------------------------------------- *)
(** *** Proof of [incr] *)

(** First, we consider the verification of the increment function, which is
    written in OCaml syntax as:

OCaml:

   let incr p =
      p := !p + 1

    Recall that, for simplicity, we assume programs to be written in "A-normal
    form", that is, with all intermediate expressions named by a let-binding.
    Thereafter, we thus consider the following definition for the [incr].

OCaml:

   let incr p =
      let n = !p in
      let m = n+1 in
      p := m

    Using the construct from our embedded programming language, the definition
    of [incr] is formalized as shown below. *)

Definition incr : val :=
  val_fun "p" (
    trm_let "n" (trm_app val_get (trm_var "p")) (
    trm_let "m" (trm_app (trm_app val_add
                             (trm_var "n")) (val_int 1)) (
    trm_app (trm_app val_set (trm_var "p")) (trm_var "m")))).

(** Alternatively, using notation and coercions, the same program can be written
    more concisely. *)

Definition incr' : val :=
  <{ fun 'p =>
       let 'n = ! 'p in
       let 'm = 'n + 1 in
       'p := 'm }>.

(** Let us check that the two definitions are indeed the same. *)

Lemma incr_eq_incr' :
  incr = incr'.
Proof using. reflexivity. Qed.

(** Recall from the first chapter the specification of the increment function.
    Its precondition requires a singleton state of the form [p ~~> n]. Its
    postcondition describes a state of the form [p ~~> (n+1)]. *)

Lemma triple_incr : forall (p:loc) (n:int),
  triple (trm_app incr p)
    (p ~~> n)
    (fun _ => p ~~> (n+1)).

(** We next show a detailed proof for this specification. It exploits:

    - the structural reasoning rules,
    - the reasoning rules for terms,
    - the specification of the primitive functions,
    - the [xsimpl] tactic for simplifying entailments.
*)

Proof using.
  (* initialize the proof *)
  intros. applys triple_app_fun. { reflexivity. } simpl.
  (* reason about [let n = ..] *)
  applys triple_let.
  (* reason about [!p] *)
  { apply triple_get. }
  (* name [n'] the result of [!p] *)
  intros n'. simpl.
  (* substitute away the equality [n' = n] *)
  apply triple_hpure. intros ->.
  (* reason about [let m = ..] *)
  applys triple_let.
  (* apply the frame rule to put aside [p ~~> n] *)
  { applys triple_conseq_frame.
    (* reason about [n+1] in the empty state *)
    { applys triple_add. }
    { xsimpl. }
    { xsimpl. } }
  (* name [m'] the result of [n+1] *)
  intros m'. simpl.
  (* substitute away the equality [m' = m] *)
  apply triple_hpure. intros ->.
  (* reason about [p := m] *)
  applys triple_conseq_frame.
    { applys triple_set. }
    { xsimpl. }
    { xsimpl. }
Qed.

(* ----------------------------------------------------------------- *)
(** *** Proof of [succ_using_incr] *)

(** Recall from [Basic] the function [succ_using_incr]. *)

Definition succ_using_incr : val :=
  <{ fun 'n =>
       let 'r = val_ref 'n in
       incr 'r;
       let 'x = ! 'r in
       val_free 'r;
       'x }>.

(** Recall the specification of [succ_using_incr]. *)

Lemma triple_succ_using_incr : forall (n:int),
  triple (trm_app succ_using_incr n)
    \[]
    (fun v => \[v = val_int (n+1)]).

(** **** Exercise: 4 stars, standard, especially useful (triple_succ_using_incr)

    Verify the function [triple_succ_using_incr]. Hint: follow the pattern of
    [triple_incr]. Use [applys triple_seq] for reasoning about a sequence. Use
    [applys triple_val] for reasoning about the final return value, namely [x].
    *)

Proof using.
intros. applys* triple_app_fun.
simpl.
applys triple_let.
apply triple_ref.
simpl.
intro r.
apply triple_hexists.
intro l.
apply triple_hpure.
intros.
subst.
applys triple_seq.
apply triple_incr.
applys triple_let.
apply triple_get.
intro x.
simpl.
applys triple_seq.
apply triple_hpure.
intros.
eapply triple_conseq.
apply triple_free.
Search "==>".
apply himpl_refl.
intro r.
apply (himpl_hempty_hpure H).
apply triple_val.
apply himpl_refl.
Qed.

(** [] *)

End ExamplePrograms.

(* ================================================================= *)
(** ** What's Next *)

(** The matter of the next chapters is to introduce additional technology to
    streamline the proof process, notably by:

    - automating the application of the frame rule,
    - eliminating the need to manipulate program variables
      and substitutions during the verification proof. *)

(* ################################################################# *)
(** * More Details *)

(** The "more details" section presents several additional reasoning rules. *)

(* ================================================================= *)
(** ** Triple for Terms with Same Semantics *)

Module ProofsSameSemantics.

(** A general principle is that if, according to the semantics, the terms [t1]
    and [t2] admits the same behaviors, then [t1] and [t2] satisfy the same
    triples. The statement can also be reformulated in an asymmetric fashion: if
    [t2] admits no more behaviors than [t1], then [t2] satisfies all the triples
    that [t1] satisfies. This section explains how to formalize this asymmetric
    statement, which is more general and more useful in practice than the
    statement expressed in terms of equivalences.

    The judgment [eval_like t1 t2] asserts that if [t1] terminates and produces
    results in [Q], then so does [t2]. *)

Definition eval_like (t1 t2:trm) : Prop :=
  forall s Q, eval s t1 Q -> eval s t2 Q.

(** **** Exercise: 1 star, standard, especially useful (eval_like_eta_expansion)

    Prove that when the relation [eval_like t1 t2] holds, any triple that [t2]
    satisfies any triple that [t1] satisfies. *)

Lemma triple_eval_like : forall t1 t2 H Q,
  eval_like t1 t2 ->
  triple t1 H Q ->
  triple t2 H Q.
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** The remaining of this section presents 4 examples applications of this
    reasoning principle. To begin with, consider the term [let x = t in x],
    represented as [trm_let x t x] for a variable named [x]. This term reduces
    to [t], hence the relation [eval_like t (trm_let x t x)] holds. *)

Lemma eval_like_eta_reduction : forall (t:trm) (x:var),
  eval_like t (trm_let x t x).
Proof using.
  introv R. applys eval_let R.
  simpl. rewrite var_eq_spec. case_if.
  introv Hv. applys eval_val Hv.
Qed.

(** **** Exercise: 4 stars, standard, optional (eval_like_eta_expansion)

    Prove that the symmetric relation [eval_like (trm_let x t x) t] also holds.
    *)

Lemma eval_like_eta_expansion : forall (t:trm) (x:var),
  eval_like (trm_let x t x) t.
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** **** Exercise: 2 stars, standard, especially useful (eta_same_triples)

    Conclude that [t] and [trm_let x t x] satisfy exactly the same set of
    triples. (This result will be exploited in chapter [Affine], for
    proving the equivalence of two versions of the garbage collection rule.) *)

Lemma eta_same_triples : forall (t:trm) (x:var) H Q,
   triple t H Q <-> triple (trm_let x t x) H Q.
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** Another application of the reasoning rule [triple_eval_like] is to revisit
    the proof of [triple_app_fun] by arguing that [trm_app (val_fun x t1) v2]
    reduces to [subst x v2 t1], hence they admit the same behavior. *)

Lemma triple_app_fun : forall x v1 v2 t1 H Q,
  v1 = val_fun x t1 ->
  triple (subst x v2 t1) H Q ->
  triple (trm_app v1 v2) H Q.
Proof using.
  introv E M1. applys triple_eval_like M1.
  introv R. applys eval_app_fun E R.
Qed.

(** A even more interesting application is a rule that allows to modify the
    parenthesis structure of a sequence, from [t1; (t2; t3)] to [(t1;t2); t3].
    Such a change in the parenthesis structure of a sequence might be helfpul to
    apply the frame rule around [t1;t2], for example. *)

(** **** Exercise: 3 stars, standard, optional (triple_trm_seq_assoc)

    Prove that the term [t1; (t2; t3)], which corresponds to the natural parsing
    of [t1; t2; t3], satisfies any triple that the term [(t1;t2); t3] satisfies.
    *)

Lemma triple_trm_seq_assoc : forall t1 t2 t3 H Q,
  triple (trm_seq (trm_seq t1 t2) t3) H Q ->
  triple (trm_seq t1 (trm_seq t2 t3)) H Q.
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

End ProofsSameSemantics.

(* ================================================================= *)
(** ** The Combined Let-Frame Rule *)

Module LetFrame.

(** Recall the Separation Logic let rule. *)

Parameter triple_let : forall x t1 t2 Q1 H Q,
  triple t1 H Q1 ->
  (forall v1, triple (subst x v1 t2) (Q1 v1) Q) ->
  triple (trm_let x t1 t2) H Q.

(** At first sight, it seems that, to reason about [let x = t1 in t2] in a state
    described by precondition [H], we need to first reason about [t1] in that
    same state. Yet, [t1] may well require only a subset of the state [H] to
    evaluate, and not all of [H].

    The "let-frame" rule combines the rule for let-bindings with the frame rule
    to make it more explicit that the precondition [H] may be decomposed in the
    form [H1 \* H2], where [H1] is the part needed by [t1], and [H2] denotes the
    rest of the state. The part of the state covered by [H2] remains unmodified
    during the evaluation of [t1], and appears as part of the precondition of
    [t2]. The corresponding statement is as follows. *)

Lemma triple_let_frame : forall x t1 t2 Q1 H H1 H2 Q,
  triple t1 H1 Q1 ->
  H ==> H1 \* H2 ->
  (forall v1, triple (subst x v1 t2) (Q1 v1 \* H2) Q) ->
  triple (trm_let x t1 t2) H Q.

(** **** Exercise: 3 stars, standard, especially useful (triple_let_frame)

    Prove the let-frame rule. *)

Proof using.
intros.
eapply triple_conseq.
applys triple_let.
apply triple_frame.
exact H0.
exact H4.
exact H3.
apply qimpl_refl.
Qed.

(** [] *)

End LetFrame.

(* ################################################################# *)
(** * Optional Material *)

(* ================================================================= *)
(** ** Alternative Specification Style for Pure Preconditions *)

Module DivSpec.

(** Recall the specification for division. *)

Parameter triple_div : forall n1 n2,
  n2 <> 0 ->
  triple (val_div n1 n2)
    \[]
    (fun r => \[r = val_int (Z.quot n1 n2)]).

(** Equivalently, we could place the requirement [n2 <> 0] in the precondition:
    *)

Parameter triple_div' : forall n1 n2,
  triple (val_div n1 n2)
    \[n2 <> 0]
    (fun r => \[r = val_int (Z.quot n1 n2)]).

(** The two presentations are equivalent. First, let us prove [triple_div']
    using [triple_div]. *)

Lemma triple_div'_from_triple_div : forall n1 n2,
  triple (val_div n1 n2)
    \[n2 <> 0]
    (fun r => \[r = val_int (Z.quot n1 n2)]).
Proof using.
  intros. applys triple_hpure'. applys triple_div.
Qed.

(** **** Exercise: 2 stars, standard, especially useful (triple_div_from_triple_div')

    Prove [triple_div] using [triple_div']. *)

Lemma triple_div_from_triple_div' : forall n1 n2,
  n2 <> 0 ->
  triple (val_div n1 n2)
    \[]
    (fun r => \[r = val_int (Z.quot n1 n2)]).
Proof using. (* FILL IN HERE *) Admitted.

(** [] *)

(** Placing pure preconditions outside of the triples makes it slightly more
    convenient to exploit specifications. For this reason, we adopt the style
    that precondition only contain the description of heap-allocated data
    structures. *)

End DivSpec.

(* ================================================================= *)
(** ** Alternative Specification Style for Result Values *)

Module MatchStyle.

(** Recall the specification for the function [ref]. *)

Parameter triple_ref : forall v,
  triple (val_ref v)
    \[]
    (fun r => \exists p, \[r = val_loc p] \* p ~~> v).

(** Its postcondition could be equivalently stated by using, instead of an
    existential quantifier, a pattern matching construct. *)

Parameter triple_ref' : forall v,
  triple (val_ref v)
    \[]
    (fun r => match r with
              | val_loc p => (p ~~> v)
              | _ => \[False]
              end).

(** However, the pattern-matching presentation is less readable and would be
    fairly cumbersome to work with in practice. *)

End MatchStyle.

(* ----------------------------------------------------------------- *)
(** *** Proof of [factorec] *)

(** This section presents a manual proof for a recursive function. *)

Module ExamplePrograms2.
Export ProgramSyntax.
Import Basic.Facto.

(** In this section, the corollary [triple_hpure'] established in the previous
    chapter, can be useful. *)

Parameter triple_hpure' : forall t (P:Prop) Q,
  (P -> triple t \[] Q) ->
  triple t \[P] Q.

(** Recall the function [repeat_incr] from chapter [Basic].

OCaml:

    let rec factorec n =
      if n <= 1 then 1 else n * factorec (n-1)
*)

Definition factorec : val :=
  <{ fix 'f 'n =>
       let 'b = ('n <= 1) in
       if 'b
         then 1
         else let 'x = 'n - 1 in
              let 'y = 'f 'x in
              'n * 'y }>.

(** **** Exercise: 4 stars, standard, especially useful (triple_factorec)

    Verify the function [factorec]. Hint: exploit [triple_app_fix] for reasoning
    about the recursive function. Use [triple_hpure'], the corollary of
    [triple_hpure]. Exploit [triple_le] and [triple_sub] and [triple_mul] to
    reason about the behavior of the primitive operations involved. Exploit
    [applys triple_if. case_if as C.] to reason about the conditional;
    alternatively, if using [triple_if_case], you'll need to use the tactic
    [rew_bool_eq in *] to simplify, e.g., the expression
    [isTrue (m <= 1)) = true]. *)

Lemma triple_factorec : forall n,
  n >= 0 ->
  triple (factorec n)
    \[]
    (fun r => \[r = facto n]).
Proof using.
  (* https://github.com/coq/coq/issues/19127 *)
Admitted.
(*
intros.
induction n.
{
applys* triple_app_fix.
simpl.
applys triple_let.
applys triple_le.
intro b.
eapply triple_hpure'.
intros.
simpl.
subst.
apply triple_if.
case_if.
apply triple_val.
apply himpl_hempty_hpure.
rewrite* facto_init.
unfold not in C.
destruct C.
math.
}
{
applys* triple_app_fix.
simpl.
*)

(** [] *)

End ExamplePrograms2.

(* ================================================================= *)
(** ** Historical Notes *)

(** [Gordon 1989] (in Bib.v) presents the first mechanization of Hoare logic in a
    proof assistant, using the HOL tool. Gordon's pioneering work was followed
    by numerous formalizations of Hoare logic, targeting various programming
    languages.

    The original presentation of Separation Logic (1999-2001) consists of a set
    of rules written down on paper. These rules were not formally described in a
    proof assistant. Nevertheless, mechanized presentation of Separation Logic
    emerged a few years later.

    [Yu, Hamid, and Shao 2003] (in Bib.v) present the CAP framework for the
    verification in Coq of assembly-level code. This framework exploits
    separation logic style specifications, with predicate for lists and list
    segments involving the separating conjunction operator.

    In parallel, [Weber 2004] (in Bib.v), advised by Nipkow, developed the first
    mechanization of the rules of Separation Logic for a while language, using
    the Isabelle/HOL tool. His presentation is quite close from the original,
    paper presentation.

    Numerous mechanized presentations of Separation Logic, targeting various
    languages (assembly, C, core-Java, ML, etc.) and using various tools
    (Isabelle/HOL, Coq, PVS, HOL4, HOL). For a detailed list, we refer to the
    last chapter of the companion notes, linked from the [Preface]. *)

(* 2024-01-03 14:19 *)
