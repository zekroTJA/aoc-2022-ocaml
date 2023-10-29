open Lib.Util

let elfs = List.map (fun vals -> List.map int_of_string vals) 
  (split_newline (read_file_lines "inputs/day01.txt")) ;;

let elf_sums = List.map sum_list elfs ;;

max_in_list elf_sums 0 |> print_int ;;

(* ----------------------------------------------------------------------- *)

print_newline () ;;

let l = List.sort (fun a b -> b - a) elf_sums in
  let rec sum_first list i n sum =
    if i = n
    then sum
    else sum_first (List.tl list) (i+1) n (List.hd list + sum)
  in
    sum_first l 0 3 0 |> print_int
;;