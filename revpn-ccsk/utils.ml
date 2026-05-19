(** Utils *)
(** range from i to j *)

let rec (--) i j = if i > j then [] else i :: i + 1 -- j

(* let (|>>) x f =  *)
(*   match x with *)
(*   | None -> None *)
(*   | Some y -> f y *)

let rec zip ls ts = 
  match ls, ts with
  | [], _ -> []
  | _, [] -> []
  |x :: xs, y :: ys -> (x,y) :: (zip xs ys)

let unzip (xs: ('a * 'b) list) : ('a list * 'b list) =
  let rec list_left xs =
    match xs with
    | [] -> []
    | (x, y) :: ys -> x :: list_left ys in
  let rec list_right xs =
    match xs with
    | [] -> []
    | (x, y) :: ys -> y :: list_right ys in
  (list_left xs, list_right xs)


(* Append disjoint union *)
let append_disj f xs = List.filter f xs, List.filter (function x -> f x |> not) xs

(* Get the n-th element of a list *)
let rec ( !! ) xs n = 
  match xs, n with
    | [], _ -> raise (Failure "get_nth")
    | _, n when (n > List.length xs) -> raise (Invalid_argument "get_nth")
    | x ::_, 0 -> x
    | x :: xs, n -> !! xs (n-1)

let init : 'a list -> 'a list = fun xs -> List.rev xs |> List.tl |> List.rev

let last : 'a list -> 'a = fun xs -> !! xs (List.length xs - 1)

(** Concat lists *)
(* let rec ( ++ ) xs ys = *)
(*   match xs with  *)
(*   | [] -> ys  *)
(*   | xs -> (init xs) ++ ((last xs) :: ys) *)


(* Lists to set *)
let rec set_of_list xs = match xs with 
  | [] -> []
  | x :: xs -> if List.mem x xs then set_of_list xs else x :: set_of_list xs

(* Binary product *)
let rec bin_prod xs ys =
  match xs with
  | [] -> []
  | x :: xs -> 
      let aux_ys a bs = if bs == [] then [] else List.map (fun b -> (a, b)) bs in
      (aux_ys x ys) @ (bin_prod xs ys)

let max_list xs = List.fold_left (max) "" xs

let min_list xs = List.fold_left (min) (max_list xs) xs

let suprime a xs = List.filter (fun x -> (x != a)) xs

let rec sort_increasing xs =
  match xs with
  | [] -> []
  | xs ->
    let m = (min_list xs) in
    let ys = suprime m xs in
    m :: sort_increasing ys

let rec sort_increasing_pair_left xs =
  let xs_left = fst (unzip xs) in
  let sort = sort_increasing xs_left in
  match xs with
  | [] -> []
  | (x,y) :: ys -> 
      if x = (!! sort 0) then
        (x,y) :: (sort_increasing_pair_left ys)
      else
        sort_increasing_pair_left (ys @ [(x,y)])


let setminus xs ys = List.filter (fun x -> not (List.mem x ys)) xs
