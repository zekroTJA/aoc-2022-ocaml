open Lib.Util

let ic = open_in "inputs/day05.txt"

let tower_lines =
  let rec build res =
    match input_line ic with "" -> List.tl res | line -> build (line :: res)
  in
  build []

let moves =
  let rec build res =
    match input_line ic with
    | line -> build (line :: res)
    | exception End_of_file ->
        close_in ic;
        List.rev res
  in
  build []

let towers =
  let rec insert lines res =
    match lines with
    | [] -> res
    | line :: rest ->
        let line = explode line in
        let rec push i l tw =
          match l with
          | [] -> List.rev tw
          | h :: t when (i - 2) mod 4 = 0 -> push (i + 1) t (h :: tw)
          | _ :: t -> push (i + 1) t tw
        in
        insert rest (push 1 line [] :: res)
  in
  List.rev (insert tower_lines [])
  |> flip_matrix
  |> List.map (fun v -> List.filter (( <> ) ' ') v)
  |> List.map List.rev

let parse_move line =
  match String.split_on_char ' ' line with
  | [ _; i; _; f; _; t ] -> (int_of_string i, int_of_string f, int_of_string t)
  | _ -> raise (Failure "invalid move pattern")

let towers_pop towers idx =
  let rec aux towers res i v =
    match towers with
    | [] -> (List.rev res, v)
    | (v :: h) :: t when i = idx -> aux t (h :: res) (i + 1) (Some v)
    | h :: t -> aux t (h :: res) (i + 1) v
  in
  aux towers [] 0 None

let towers_push towers idx v =
  let rec aux towers res i =
    match towers with
    | [] -> List.rev res
    | h :: t when i = idx -> aux t ((v :: h) :: res) (i + 1)
    | h :: t -> aux t (h :: res) (i + 1)
  in
  aux towers [] 0

let rec move_one_by_one towers n f t =
  let move towers =
    let towers, v = towers_pop towers (f - 1) in
    match v with
    | Some v -> towers_push towers (t - 1) v
    | None ->
        raise (Failure (Printf.sprintf "did not pop anything (%i -> %i)" f t))
  in
  if n = 0 then towers else move_one_by_one (move towers) (n - 1) f t

let get_top_items towers =
  let rec aux towers items =
    match towers with
    | [] -> List.rev items
    | (i :: _) :: t -> aux t (i :: items)
    | _ -> raise (Failure "empty tower")
  in
  aux towers []

let () =
  let res_towers =
    let moves = moves |> List.map parse_move in
    let rec do_move moves res =
      match moves with
      | [] -> res
      | (n, f, t) :: moves -> do_move moves (move_one_by_one res n f t)
    in
    do_move moves towers
  in
  res_towers |> get_top_items |> List.iter (fun v -> Printf.printf "%c" v)

(* --------------------------------------------------------------------------------- *)

let () = print_newline ()

let move_multiple_by_one towers n f t =
  let rec move towers n res =
    if n = 0 then (towers, res)
    else
      let towers, v = towers_pop towers (f - 1) in
      match v with
      | None -> raise (Failure "could not pop anything")
      | Some v -> move towers (n - 1) (v :: res)
  in
  let rec push towers items =
    match items with
    | [] -> towers
    | h :: rest -> push (towers_push towers (t - 1) h) rest
  in
  let towers, items = move towers n [] in
  push towers items

let () =
  let res_towers =
    let moves = moves |> List.map parse_move in
    let rec do_move moves res =
      match moves with
      | [] -> res
      | (n, f, t) :: moves -> do_move moves (move_multiple_by_one res n f t)
    in
    do_move moves towers
  in
  res_towers |> get_top_items |> List.iter (fun v -> Printf.printf "%c" v)
