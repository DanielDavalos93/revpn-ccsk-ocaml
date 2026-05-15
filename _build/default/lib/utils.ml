(** range from i to j *)

let (--) i j = 
  let rec aux n acc =
      if n < i then acc else aux (n-1) (n :: acc)
    in aux j [] ;;


(** range from 0 to j*)

let range j = 
  match j with  
  | 0 -> []
  | i -> 0--(i-1);;

let (|>>) x f = 
  match x with
  | None -> None
  | Some y -> f y


