#!/usr/bin/env ocaml

let n = int_of_string (Sys.argv.(1))

let () =
  Format.printf {|{"action": "add_items", "items":[|};
  for i=14 to n do
      Format.printf {|
      {
        "category": "phone",
        "attrs": {
            "manufacturer": "Apple",
            "name": "iPhone %d",
            "year": %d,
            "mobileos": "iOS",
            "screen_size": %f
        }
      } |} i (2009+i) (2.3 *. log (float_of_int i));
      if i <> n then Format.printf ","
  done;
  Format.printf {|]}|}

