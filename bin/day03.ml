open Lib.Util

let rucksacks = read_file_lines "inputs/day03.txt" |> List.map explode
let split_half list = take list (List.length list / 2)

let priority v =
  match v with
  | 'a' .. 'z' -> Char.code v - 96
  | 'A' .. 'Z' -> Char.code v - 65 + 27
  | _ -> raise (Failure "invalid character")

let comp_dupes = function
  | comp_a, comp_b ->
      find_duplicates comp_a comp_b |> List.map priority |> sum_list

let () =
  List.map split_half rucksacks |> List.map comp_dupes |> sum_list |> print_int

(* ------------------------------------------------------------------------------ *)

let () = print_newline ()

let rec first_contained_in_all : 'a list list -> 'a option = function
  | [] -> None
  | first :: others -> (
      match first with
      | [] -> None
      | h :: t ->
          if contained_in_all others h then Some h
          else first_contained_in_all (t :: others))

let () =
  group rucksacks 3
  |> List.map first_contained_in_all
  |> List.map Option.get |> List.map priority |> sum_list |> print_int
