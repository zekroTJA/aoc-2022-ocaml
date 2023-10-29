let read_file_lines file_name =
  let ic = open_in file_name in
  let rec build_list l =
    match input_line ic with
    | line -> build_list (line :: l)
    | exception End_of_file -> close_in ic; List.rev l in
  build_list []
;;


let split_newline list =
  let rec split lines stash result =
    match lines with
      | [] -> (List.rev (stash :: result))
      | "" :: rest -> split rest [] (List.rev stash :: result)
      | c :: rest -> split rest (c :: stash) result
  in
    split list [] [] 
;;

let rec sum_list list =
   match list with
    | [] -> 0
    | h :: t -> h + (sum_list t)
;;

let rec max_in_list list mx =
  match list with
    | [] -> mx
    | h :: t -> max_in_list t (max h mx)
;;