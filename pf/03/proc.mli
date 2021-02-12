(* Ten plik, razem z plikiem proc.ml implementują prostą bibliotekę do
  lekkich, kooperatywnych wątków. Aby używać tej biblioteki trzeba ją
  najpierw skompilować za pomocą poleceń:

  $ ocamlc -c proc.mli
  $ ocamlc -c proc.ml

  i ewentualnie, gdy ktoś chce z niej korzystać w programach kompilowanych
  do kodu maszynowego:

  $ ocamlopt -c proc.ml

  Aby załadować bibliotekę do interpretera, przekaż odpowiedni plik cmo
  jako argument interpretera w wierszu poleceń:

  $ utop proc.cmo

  Implementowane w niej funkcje są widoczne w module Proc:

  # Proc.run (Proc.send "ABC") ;;
  ABC
  - : unit = ()

  Można też moduł Proc otworzyć:

  # open Proc ;;
  # run (send "ABC") ;;
  ABC
  - : unit = ()
*)

(** Abstrakcyjny typ odpowiedzi *)
type ('z,'i,'o) ans

(** Obliczenie w CPS-ie wewnątrz pojedynczego procesu.
  Ma cztery parametry:
  'a - typ obliczenia (typ oczekiwany przez kontynuację)
  'z - typ zwracanej wartości przez cały proces
  'i - typ elementów strumienia wejściowego
  'o - typ elementów strumienia wyjściowego *)
type ('a,'z,'i,'o) proc = ('a -> ('z,'i,'o) ans) -> ('z,'i,'o) ans

(** Wysyłanie pojedynczej wartości do strumienia wyjściowego *)
val send : 'o -> (unit,'z,'i,'o) proc

(** Odebranie pojedynczej wartości ze strumienia wejściowego *)
val recv : ('i,'z,'i,'o) proc

(** Zastępuje bieżący proces dwoma nowymi połączonymi ze sobą *)
val (<|>>) : ('z,'z,'i,'m) proc -> ('z,'z,'m,'o) proc -> ('a,'z,'i,'o) proc

(** Uruchamia proces. Na wejściu znajdują się kolejne wiersze wejścia
  standardowego, natomiast wysyłane napisy są przekazywane na wyjście
  standardowe. *)
val run : ('a,'a,string,string) proc -> 'a
