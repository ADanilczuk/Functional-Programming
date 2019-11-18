
(*----- 2 --------*)
type move = TRANSFER of int * int | DRAIN of int | FILL of int;;
type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;
exception Error;;

let fill pair n =
  let rec fl xs n elem =
    match xs with
    |[] -> []
    |hd::tl -> if n= 0 then elem::tl
               else hd::(fl tl (n-1) elem)
  in
  fl (snd pair) n (List.nth (fst pair) n);;

fill ([3;5;4],[0;0;0]) 1;;

let drain pair n=
  let rec dr xs n =
    match xs with
    |[]-> []
    |hd::tl -> if n=0 then (0)::tl
               else hd::(dr tl (n-1))
  in
  (dr (snd pair) n);;

drain ([3;5;4],[1;3;0]) 1;;

let transfer pair a b =
  let b_max = List.nth (fst pair) b in
  let b_actual = List.nth (snd pair) b in
  let a_actual = List.nth (snd pair) a in
  
  let rec change xs n=
    match xs with
    |[]->[]
    |hd::tl -> if n=0 then (if b_actual+a_actual>b_max then b_max::tl
                            else (b_actual+a_actual)::tl)
               else hd::(change tl (n-1))
  in
                 
  let rec tr xs a b=
    match xs with
    |[]->[]
    |hd::tl -> match (a,b) with
               |(0,(-1))  -> if hd+b_actual < b_max+1 then (0)::tl
                             else (hd-(b_max - b_actual))::tl
               |(0, _) -> if hd+b_actual < b_max+1 then (0)::(change tl (b-1))
                          else (hd-(b_max - b_actual))::(change tl (b-1))
               |(_, 0) -> tr  (change xs 0) a (-1)
               |(_, _) -> hd::(tr tl (a-1) (b-1)) 
  in
  if a=b then (snd pair)
  else tr (snd pair) a b;;


transfer ([3;5;4],[1;3;0]) 0 1;;

transfer ([3;5;4],[1;3;0]) 1 0;;
transfer ([3;5;4],[1;3;0]) 0 0;;
transfer ([3;5;4],[1;3;0]) 1 2;;


let fill_all pair steps =
  let rec all_glasses it xs acc=
    match xs with
    | [] -> acc
    | l::ls -> all_glasses (it+1) ls (((fill pair it), (FILL(it)::steps))::acc)
  in all_glasses 0 (snd pair) [];;


let drain_all pair steps =
  let rec all_glasses it xs acc=
    match xs with
    | [] -> acc
    | l::ls -> all_glasses (it+1) ls (((drain pair it), (DRAIN(it)::steps))::acc)
  in all_glasses 0 (snd pair) [];;
 
let transfer_all pair steps =
  let len = (List.length (snd pair)) - 1 in
    let rec all_pairs nr1 nr2 acc =
      match (nr1, nr2) with
      | (x,y) when x = len -> acc
      | (x,y) when x < len && y < len -> all_pairs nr1 (nr2+1) ((transfer pair x y, TRANSFER(x,y)::steps)::acc)
      | (x,y) when x < len && y = len -> all_pairs (nr1+1) 0 ((transfer pair x y, TRANSFER(x,y)::steps)::acc)
    in all_pairs 0 0 [];;


let is_volume pair volume =
  let rec hlp xs =
    match xs with
    | [] -> false
    | x::xs -> if x = volume then true else hlp xs
  in hlp (fst pair);;
