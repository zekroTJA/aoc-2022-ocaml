let read_file_lines file_name =
  let ic = open_in file_name in
  let rec build_list l =
    match input_line ic with
    | line -> build_list (line :: l)
    | exception End_of_file ->
        close_in ic;
        List.rev l
  in
  build_list []

let read_file file_name =
  let ic = open_in file_name in
  let rec read_to_end res =
    match input_char ic with
    | c -> read_to_end (c :: res)
    | exception End_of_file ->
        close_in ic;
        List.rev res
  in
  read_to_end []

let split_newline list =
  let rec split lines stash result =
    match lines with
    | [] -> List.rev (stash :: result)
    | "" :: rest -> split rest [] (List.rev stash :: result)
    | c :: rest -> split rest (c :: stash) result
  in
  split list [] []

let rec sum_list list = match list with [] -> 0 | h :: t -> h + sum_list t

let rec max_in_list list mx =
  match list with [] -> mx | h :: t -> max_in_list t (max h mx)

let explode str =
  let rec exp i res = if i < 0 then res else exp (i - 1) (str.[i] :: res) in
  exp (String.length str - 1) []

let take list n =
  let rec take first rest i =
    if i = n then (List.rev first, rest)
    else
      match rest with
      | [] -> (List.rev first, rest)
      | head :: tail -> take (head :: first) tail (i + 1)
  in
  take [] list 0

let find_duplicates list_a list_b =
  let rec find list dupes =
    match list with
    | [] -> dupes
    | head :: tail ->
        if
          List.exists (( = ) head) list_b
          && not (List.exists (( = ) head) dupes)
        then find tail (head :: dupes)
        else find tail dupes
  in
  find list_a []

let group list group_size =
  let rec group list stash res i =
    if i = group_size then group list [] (List.rev stash :: res) 0
    else
      match list with
      | [] -> List.rev res
      | head :: tail -> group tail (head :: stash) res (i + 1)
  in
  group list [] [] 0

let contained_in_all lists v =
  let rec contained lists v ok =
    if not ok then false
    else
      match lists with
      | [] -> ok
      | h :: t -> contained t v (List.exists (( = ) v) h)
  in
  contained lists v true

let split_once sep v =
  let split = String.split_on_char sep v in
  match split with
  | [] -> ("", "")
  | [ a ] -> (a, "")
  | h :: t -> (h, String.concat (Core.Char.to_string sep) t)

let get_idx list idx =
  let rec get_idx list idx i =
    match list with
    | [] -> raise (Failure "invalid idx")
    | h :: _ when i = idx -> h
    | _ :: t -> get_idx t idx (i + 1)
  in
  get_idx list idx 0

let flip_matrix = function
  | [] -> []
  | first :: rest ->
      let width = List.length first in
      let rec build_mx j res_mx =
        if j = width then List.rev res_mx
        else
          let rec build_col lines col col_idx i =
            match lines with
            | [] -> List.rev col
            | h :: t -> build_col t (get_idx h col_idx :: col) col_idx (i + 1)
          in
          build_mx (j + 1) (build_col (first :: rest) [] j 0 :: res_mx)
      in
      build_mx 0 []

let is_all_unique s =
  let rec aux = function
    | [] -> true
    | h :: _ when List.filter (( = ) h) s |> List.length > 1 -> false
    | _ :: t -> aux t
  in
  aux s

(* ---- TESTS ---------------------------------------------------------------------------------- *)

let%test_unit "take" =
  [%test_eq: Base.int Base.list * Base.int Base.list]
    (take [ 1; 2; 3; 4; 5 ] 2)
    ([ 1; 2 ], [ 3; 4; 5 ]);
  [%test_eq: Base.int Base.list * Base.int Base.list]
    (take [ 1; 2 ] 3)
    ([ 1; 2 ], [])

let%test_unit "find_duplicates" =
  [%test_eq: Base.int Base.list] (find_duplicates [ 1; 2; 3 ] [ 2; 4; 5 ]) [ 2 ];
  [%test_eq: Base.int Base.list]
    (find_duplicates [ 1; 2; 3 ] [ 1; 2; 3 ])
    [ 3; 2; 1 ];
  [%test_eq: Base.int Base.list]
    (find_duplicates [ 1; 2; 2; 3 ] [ 1; 2; 2; 3 ])
    [ 3; 2; 1 ]

let%test_unit "group" =
  [%test_eq: Base.int Base.list Base.list]
    (group [ 1; 2; 3; 4; 5; 6; 7; 8 ] 2)
    [ [ 1; 2 ]; [ 3; 4 ]; [ 5; 6 ]; [ 7; 8 ] ]

let%test_unit "contained_in_all" =
  [%test_eq: Base.bool] (contained_in_all [ [ 1; 2; 3 ] ] 2) true;
  [%test_eq: Base.bool]
    (contained_in_all [ [ 1; 2; 3 ]; [ 2; 4 ]; [ 3; 2; 5 ] ] 2)
    true;
  [%test_eq: Base.bool]
    (contained_in_all [ [ 1; 2; 3 ]; [ 2; 4 ]; [ 3; 2; 5 ] ] 3)
    false

let%test_unit "split_once" =
  [%test_eq: Base.string * Base.string]
    (split_once ',' "hello,world")
    ("hello", "world");
  [%test_eq: Base.string * Base.string]
    (split_once ',' "helloworld")
    ("helloworld", "");
  [%test_eq: Base.string * Base.string]
    (split_once ',' "hello,world,whats,up")
    ("hello", "world,whats,up");
  [%test_eq: Base.string * Base.string] (split_once ',' "") ("", "")

let%test_unit "get_idx" =
  [%test_eq: Base.int] (get_idx [ 1; 2; 3 ] 1) 2;
  [%test_eq: Base.int] (get_idx [ 1; 2; 3 ] 0) 1

let%test_unit "flip_matrix" =
  [%test_eq: Base.int Base.list Base.list]
    (flip_matrix [ [ 1; 2 ]; [ 3; 4 ] ])
    [ [ 1; 3 ]; [ 2; 4 ] ];
  [%test_eq: Base.int Base.list Base.list]
    (flip_matrix [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ])
    [ [ 1; 4; 7 ]; [ 2; 5; 8 ]; [ 3; 6; 9 ] ]

let%test_unit "is_all_unique" =
  [%test_eq: Base.bool] (is_all_unique [ 1; 2 ]) true;
  [%test_eq: Base.bool] (is_all_unique [ 1; 2; 3; 4 ]) true;
  [%test_eq: Base.bool] (is_all_unique [ 1; 2; 1 ]) false;
  [%test_eq: Base.bool] (is_all_unique [ 1; 2; 3; 2; 3; 1 ]) false
