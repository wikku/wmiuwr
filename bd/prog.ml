module J = Yojson.Safe
open Pgx
open Pgx_unix

[@@@warning "-partial-match"]

(* let unix_domain_socket_dir = "/tmp" *)
let unix_domain_socket_dir = "/run/postgresql/"

let conn = connect ~unix_domain_socket_dir ()

let input = J.from_channel stdin

let value_of_json j = Value.of_string (J.to_string j)
let value_of_json_string j = Value.of_string (J.Util.to_string j)
let value_of_json_int j = Value.of_int (J.Util.to_int j)

let int_of_value v = Option.get (Value.to_int v)
let string_of_value v = Option.get (Value.to_string v)
let json_of_value v =
  let s = string_of_value v in
  try J.from_string s with Yojson.Json_error _ -> `String s

let ( .%[] ) obj key = J.Util.member key obj

let success payload =
  J.Util.combine (`Assoc ["status", `String "success"]) (`Assoc payload)

let failure payload =
  J.Util.combine (`Assoc ["status", `String "failure"]) (`Assoc payload)


let paren s = Format.sprintf "(%s)" s

let print_numeric = function
  | `Int i -> paren @@ string_of_int i
  | `String s -> paren @@ Format.sprintf "(i_attrs->>'%s')::numeric" s
  | `List [`String ("MAX" | "AVG" | "MIN" as op); `String field] ->
    paren @@ Format.sprintf "SELECT %s((i_attrs->>'%s')::numeric) FROM item" op field
  | #J.t -> failwith "print_numeric"

let print_text = function
  | `String s when String.starts_with ~prefix:"'" s
      && String.ends_with ~suffix:"'" s ->
    paren @@ s
  | `String s -> paren @@ Format.sprintf "(i_attrs->>'%s')" s
  | #J.t -> failwith "print_text"

let rec print_condition j = match J.Util.to_list j with
  | `String "AND" :: args ->
    paren @@ String.concat " AND " (List.map print_condition args)
  | `String "OR" :: args ->
    paren @@ String.concat " OR " (List.map print_condition args)
  | `String ("<" | "<=" | ">=" | ">" as op) :: args when List.length args = 2 ->
    paren @@ String.concat op (List.map print_numeric args)
  | `String ("<>" | "=" as op) :: args when List.length args = 2 ->
    paren @@ String.concat op (List.map print_text args)

let _ =
  let open J.Util in
  (* print_endline (J.to_string input); *)
  print_endline @@ J.to_string @@ match to_string input.%["action"] with
  | "add_or_extend_parameter" ->
    let [[id]] =
      execute
       ~params:[value_of_json_string input.%["name"]; value_of_json input.%["type"]]
       conn "INSERT INTO parameter (p_name, p_type) VALUES ($1, $2)
             ON CONFLICT (p_name) DO
             UPDATE SET p_type = EXCLUDED.p_type
             WHERE jsonb_typeof(parameter.p_type) = jsonb_typeof(EXCLUDED.p_type)
             AND parameter.p_type <@ EXCLUDED.p_type
             RETURNING p_id"
    in
    success ["id", `Int (int_of_value id)]
  | "add_category" ->
    let [[name]] =
      execute
       ~params:[value_of_json_string input.%["name"]]
       conn "INSERT INTO category (c_name) VALUES ($1) RETURNING c_name"
    in
    success ["name", `String (string_of_value name)]
  | "add_items" ->
    let items = to_list input.%["items"] in
    let placeholders =
      List.mapi (fun i _ -> Format.sprintf "($%d,$%d)" (2*i+1) (2*i+2)) items
    and params =
      List.concat_map
       (fun item ->
        [value_of_json_string item.%["category"];
         value_of_json item.%["attrs"] ])
       items
    in
    let query =
      "INSERT INTO item (i_category, i_attrs) VALUES "
      ^ String.concat "," placeholders ^ " RETURNING i_id"
    in
    success ["ids", `List (List.map
      (fun [item] -> `Int (int_of_value item))
      (execute ~params:params conn query))]
  | "modify_items" ->
    let items = to_list input.%["items"] in
    let placeholders =
      List.mapi
       (fun i _ -> Format.sprintf "($%d,$%d,$%d)" (3*i+1) (3*i+2) (3*i+3))
       items
    and params =
      List.concat_map
       (fun item ->
        [value_of_json_int item.%["id"];
         value_of_json_string item.%["category"];
         value_of_json item.%["attrs"] ])
       items
    in
    let query =
      "UPDATE item as i SET
      i.i_category = v.i_category, i.i_attrs = v.i_attrs
      FROM (VALUES " ^ String.concat "," placeholders ^ "
      ) AS v(i_id, i_category, i_attrs)
      WHERE i.i_id = v.i_id
      RETURNING i_id"
    in
    success ["ids", `List (List.map
      (fun [item] -> `Int (int_of_value item))
      (execute ~params:params conn query))]
  | "delete_item" ->
    let [[id]] =
      execute
       ~params:[value_of_json_int input.%["id"]]
       conn "DELETE FROM item WHERE i_id = $1 RETURNING i_id";
    in
    success ["id", `Int (int_of_value id)]
  | "query_items" ->
    let condition = match input.%["where"] with
    | `Null -> "TRUE"
    | j -> print_condition j
    in
    let group = match input.%["group"] with
    | `Null | `Bool false -> false
    | `Bool true -> true
    in
    let select = match input.%["aggregate"] with
    | `Null -> ["i_id"; "i_category"; "i_attrs"]
    | `List l ->
      let l = List.map to_string l in
      if group then "i_category" :: l else l
    in
    let query =
      Format.sprintf
      "SELECT %s\nFROM item\nWHERE %s\n%s"
      (String.concat "," select)
      condition
      (if group then "GROUP BY i_category" else "")
    in
    (* output_string stderr query; *)
    let result = execute conn query in
    success @@ ["results",
      `List (List.map
              (fun row -> `Assoc (List.map2
                                   (fun c v -> c, json_of_value v)
                                   select row))
              result)]
  | _ -> failure []
