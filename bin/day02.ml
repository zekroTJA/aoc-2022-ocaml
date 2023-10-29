open Lib.Util

let split_space str =
  let split = function
    | a :: _ :: c :: _ -> (a, c)
    | _ -> raise (Failure "invalid value to split")
  in
    split (explode str)
;;

let score v =
  match v with
    | ('A', 'X') -> 1 + 3
    | ('A', 'Y') -> 2 + 6
    | ('A', 'Z') -> 3 + 0
    | ('B', 'X') -> 1 + 0
    | ('B', 'Y') -> 2 + 3
    | ('B', 'Z') -> 3 + 6
    | ('C', 'X') -> 1 + 6
    | ('C', 'Y') -> 2 + 0
    | ('C', 'Z') -> 3 + 3
    | _ -> raise (Failure "invalid combination")
;;

let lines = read_file_lines "inputs/day02.txt" |>
  List.map split_space ;;

List.map score lines |> sum_list |> print_int ;;

(* ----------------------------------------------------------------------- *)

print_newline () ;;

let shape v =
  match v with
    | ('A', 'X') -> 0 + 3
    | ('A', 'Y') -> 3 + 1
    | ('A', 'Z') -> 6 + 2
    | ('B', 'X') -> 0 + 1
    | ('B', 'Y') -> 3 + 2
    | ('B', 'Z') -> 6 + 3
    | ('C', 'X') -> 0 + 2
    | ('C', 'Y') -> 3 + 3
    | ('C', 'Z') -> 6 + 1
    | _ -> raise (Failure "invalid combination")
;;

List.map shape lines |> sum_list |> print_int ;;