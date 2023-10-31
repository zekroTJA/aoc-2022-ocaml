val read_file_lines : string -> string list
(** [read_file_lines file_name] Reads contents of file [file_name] and returns the result split by line endings. *)

val split_newline : string list -> string list list
(** Takes a list of strings and splits it into sub lists on each empty string value (not including).*)

val sum_list : int list -> int
(** Takes a list of integers and returns the sum of all values. *)

val max_in_list : 'a list -> 'a -> 'a
(** [max_in_list list start_mx] 
    Takes a [list] and returns the maximum value taking [start_mx] as
    comparison base. Therefore, [start_mx] should be the lowest possible
    value in [list].*)

val explode : string -> char list
(** Takes a string and returns a list of characters it consists of. *)

val take : 'a list -> int -> 'a list * 'a list
(** [take list n] Pops the first [n] elements from [list] and returns
    them as first value. The second list contains the rest of [list].*)

val find_duplicates : 'a list -> 'a list -> 'a list
(** [find_duplicates list_a list_b] 
    Returns a list of values that are contained in [list_a] as well as
    in [list_b] (retaining duplicate values).*)

val group : 'a list -> int -> 'a list list
(** [group list group_size] Takes every [group_size] elements from [list]
    and returns them in a list of lists.*)

val contained_in_all : 'a list list -> 'a -> bool
(** [contained_in_all lists v] Returns true if [v] is contained in all
    passed [lists].*)

val split_once : char -> string -> string * string
(** [split_once sep v] Takes a string [v] and splits it once into two strings
    at character [sep].*)