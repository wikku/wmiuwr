open Logic

type context = (string * formula) list
type goalDesc = context * formula

type proof
type goal

(** Zamienia ukończony dowód na twierdzenie *)
val qed : proof -> theorem

(** Zwraca liczbę pozostałych w dowodze celów (0 <-> dowód jest gotowy i można go zakończyć) *)
val numGoals : proof -> int

(** Zwraca listę celów w danym dowodzie *)
val goals : proof -> goalDesc list


(** Tworzy pusty dowód podanego twierdzenia *)
val proof : context -> formula -> goal

(** Zwraca (assm, phi), gdzie assm oraz phi to odpowiednio dostępne
    założenia oraz formuła do udowodnienia w aktywnym podcelu *)
val goal : goal -> goalDesc

(** Ustawia aktywny cel w danym dowodzie *)
val focus : int -> proof -> goal

(** Zapomina informację o celu *)
val unfocus : goal -> proof

(** Zmienia (cyklicznie) aktywny cel na następny/poprzedni *)
val next : goal -> goal
val prev : goal -> goal

(** Wywołanie intro name pf odpowiada regule wprowadzania implikacji.
  To znaczy aktywna dziura wypełniana jest regułą:

  (nowy aktywny cel)
   (name,ψ) :: Γ ⊢ φ
   -----------------(→I)
       Γ ⊢ ψ → φ

  Jeśli aktywny cel nie jest implikacją, wywołanie kończy się błędem *)
val intro : string -> goal -> goal

(** Wywołanie apply ψ₀ pf odpowiada jednocześnie eliminacji implikacji
  i eliminacji fałszu. Tzn. jeśli do udowodnienia jest φ, a ψ₀ jest
  postaci ψ₁ → ... → ψn → φ to aktywna dziura wypełniana jest regułami
  
  (nowy aktywny cel) (nowy cel)
        Γ ⊢ ψ₀          Γ ⊢ ψ₁
        ----------------------(→E)  (nowy cel)
                ...                   Γ ⊢ ψn
                ----------------------------(→E)
                            Γ ⊢ φ

  Natomiast jeśli ψ₀ jest postaci ψ₁ → ... → ψn → ⊥ to aktywna dziura
  wypełniana jest regułami

  (nowy aktywny cel) (nowy cel)
        Γ ⊢ ψ₀          Γ ⊢ ψ₁
        ----------------------(→E)  (nowy cel)
                ...                   Γ ⊢ ψn
                ----------------------------(→E)
                            Γ ⊢ ⊥
                            -----(⊥E)
                            Γ ⊢ φ *)
val apply : formula -> goal -> goal

(** Wywołanie `apply_thm thm pf` działa podobnie do
    `apply (Logic.consequence thm) pf`, tyle że aktywna dziura od razu
   jest wypełniana dowodem thm, a otrzymane drzewo nie jest
   sfokusowane na żadnym z celów.
*)

val apply_thm : theorem -> goal -> proof

(** Wywołanie `apply_assm name pf` działa tak jak
    `apply (Logic.by_assumption f) pf`, gdzie f jest założeniem o
    nazwie name
 *)
val apply_assm : string -> goal -> proof

val pp_print_proof : Format.formatter -> proof -> unit
val pp_print_goal  : Format.formatter -> goal -> unit

val intro_forall : var -> goal -> goal
val generalize : term -> goal -> goal
