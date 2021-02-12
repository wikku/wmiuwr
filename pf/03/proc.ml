(* 'z oznacza typ ostatecznego wyniku obliczenia CPS *)

(* typ kanałów wejścia produkujących wartości typu 'i *)
type ('z,'i) in_channel =
  (* kanałem wejścia jest coś, co może przesłać 'i do dowolnego kanału
   * wyjścia *)
| In of (('z,'i) out_channel -> 'z)

(* typ kanałów wyjścia przyjmujących wartości typu 'i *)
and ('z,'o) out_channel =
  (* kanałem wyjścia jest coś, co przyjąwszy 'o, może je przesłać dalej do
   * dowolnego kanału wejścia *)
| Out of ('o -> ('z,'o) in_channel -> 'z)

type ('z,'i,'o) ans = ('z,'i) in_channel -> ('z,'o) out_channel -> 'z
(* kompletne obliczenie CPS - po otrzymaniu kanału wejścia i kanału wyjścia
 * obliczy się do wartości typu 'z (lub zapętli w nieskończoność) *)

(* z typu wnioskujemy, że proces umie wyprodukować wartość typu 'a i podać ją
 * do sparametryzowanego obliczenia CPS *)
type ('a,'z,'i,'o) proc = ('a -> ('z,'i,'o) ans) -> ('z,'i,'o) ans

(* send : 'o -> (unit,'z,'i,'o) proc *)
let send o k in_ch (Out sendch) =
  (* do kanału wyjścia przesyłamy o i podajemy kanał wejścia, który do
   * kontynuacji nie podaje informacji (unit) i podaje dalej kanały, które
   * otrzymaliśmy *)
  sendch o (In(fun out_ch -> k () in_ch out_ch))

(* recv : ('i,'z,'i,'o) proc *)
let recv k (In recvch) out_ch =
  (* do kanału wejścia podajemy kanał wyjścia, który otrzymaną wartość podaje
   * do kontynuacji i podaje dalej kanały, które otrzymaliśmy *)
  recvch (Out(fun v in_ch -> k v in_ch out_ch))

(* exit_k jest używany jako argument dla procesu, czyli
 * exit_k : 'a -> ('z, 'i, 'o) ans
 * exit_k : 'a -> ('z,'i) in_channel -> ('z,'o) out_channel -> 'z
 * ignoruje kanały i kończy obliczenie CPS zwracając (x : 'a) *)
let exit_k x _ _ = x

(* (<|>>) : ('z,'z,'i,'m) proc -> ('z,'z,'m,'o) proc -> ('a,'z,'i,'o) proc *)
let (<|>>) p1 p2 _ in_ch out_ch =
  (* ignorujemy  ^ podaną kontynuację *)
  (* po p2 kończymy obliczenie CPS *)
  p2 exit_k
    (* ale podajemy kanał wejścia, który dla otrzymanego kanału wyjścia m_ch
     * oblicza p2 z kanałami in_ch i m_ch *)
    (In(fun m_ch -> p1 exit_k in_ch m_ch))
    out_ch

let rec stdin_ch =
  (* kanał wejścia, który do otrzymanego kanału wyjścia wysyła wczytaną
   * linijkę i podaje siebie jako kontynuację *)
  In (fun (Out sendch) -> sendch (read_line ()) stdin_ch)

let rec stdout_ch =
  (* kanał wyjścia, który wypisuje otrzymaną linijkę i podaje siebie jako
   * kontynuację do otrzymanego kanału wejścia *)
  Out (fun str (In recvch) -> print_endline str; recvch stdout_ch)

(* run : ('a,'a,string,string) proc -> 'a *)
let run p = p exit_k stdin_ch stdout_ch
