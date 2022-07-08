#!/usr/bin/env ocaml
let () = Format.printf {|{"action": "delete_item", "id": %s}|} (Sys.argv.(1))
