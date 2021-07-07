let ctz n =
    let rec aux n acc =
        if Int.logand n 1 = 1
        then acc
        else aux (Int.shift_right_logical n 1) (acc+1)
    in
    aux n 0

let popcount n =
    let rec aux n acc =
        if n = 0 then acc
        else let halfn = Int.shift_right_logical n 1 in
        aux halfn (acc + Int.logand n 1)
    in
    aux n 0

let _ =
    let m = Scanf.scanf " %d " Fun.id in
    let tbl = Hashtbl.create m in
    for _ = 1 to m do
        let d, n = Scanf.scanf " %d %d " (fun d n -> d, n) in
        let e = ctz d in
        let f = Int.shift_right_logical d e in
        let a = Int.shift_left n e in
        let newval = Hashtbl.find_opt tbl f |> Option.value ~default:0 |> (+) a in
        Hashtbl.replace tbl f newval
    done;
    let result = Hashtbl.fold (fun _ v acc -> acc + popcount v) tbl 0 in
    Printf.printf "%d\n" result
    (* ; Gc.print_stat stdout *)
