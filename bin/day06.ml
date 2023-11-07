open Lib.Util

let buffer =
  let ic = open_in "inputs/day06.txt" in
  let rec read_to_end res =
    match input_char ic with
    | c -> read_to_end (c :: res)
    | exception End_of_file -> List.rev res
  in
  read_to_end []

let find_start buffer window_size =
  let rec aux buf win i =
    match (buf, win) with
    | [], _ -> raise (Failure "no start sequence found")
    | h :: t, [] -> aux t (win @ [ h ]) (i + 1)
    | h :: t, w when List.length w < window_size -> aux t (win @ [ h ]) (i + 1)
    | _, w when is_all_unique w -> i
    | h_buf :: t_buf, _ :: t_win -> aux t_buf (t_win @ [ h_buf ]) (i + 1)
  in
  aux buffer [] 0

let () = find_start buffer 4 |> print_int

(* ------------------------------------------------------------------------------------------- *)

let () = print_newline ()
let () = find_start buffer 14 |> print_int
