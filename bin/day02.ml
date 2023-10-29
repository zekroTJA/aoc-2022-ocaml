open Lib.Util

let split_space str =
  let split : char list -> char * char = function
    | a :: _ :: c :: _ -> (a, c)
    | _ -> raise (Failure "invalid value to split")
  in
    split (explode str)
;;

let lines = read_file_lines "inputs/day02.txt" |>
  List.map split_space ;;

List.iter (fun (a, b) -> Printf.printf "%c; %c\n" a b) lines