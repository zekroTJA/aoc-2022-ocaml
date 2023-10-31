open Lib.Util

let split_ranges = function a, b -> (split_once '-' a, split_once '-' b)
let parse_range = function s, e -> (int_of_string s, int_of_string e)
let parse_ranges = function s, e -> (parse_range s, parse_range e)

let sections =
  read_file_lines "inputs/day04.txt"
  |> List.map (fun v -> split_once ',' v)
  |> List.map split_ranges |> List.map parse_ranges

let overlaps_full = function
  | (a_s, a_e), (b_s, b_e) ->
      (a_s <= b_s && a_e >= b_e) || (b_s <= a_s && b_e >= a_e)

let () = sections |> List.filter overlaps_full |> List.length |> print_int

(* ------------------------------------------------------------------------------------- *)

let () = print_newline ()

let overlaps_partial = function
  | (a_s, a_e), (b_s, b_e) ->
      (a_e >= b_s && a_e <= b_e) || (b_e >= a_s && b_e <= a_e)

let () = sections |> List.filter overlaps_partial |> List.length |> print_int