(* przyklady *)

type 'a llist = LNil | LCons of 'a * (unit -> 'a llist);;

let lhd = function
  LNil -> failwith "lhd"
  | LCons (x, _) -> x;;

let ltl = function
  LNil -> failwith "lhd"
  |LCons (_, xf) -> xf();;

let rec lfrom k = LCons (k, function () -> lfrom (k+1));;

let rec ltake = function
  (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x, xf)) -> x::ltake(n-1, xf());;


let rec toLazyList = function
  [] -> LNil
  | x::xs -> LCons(x, function() -> toLazyList xs);;

let rec lmap f = function
  LNil -> LNil
  | LCons(x, xf) -> LCons(f x, function() -> lmap f (xf()) );;



(* Zadanie 1 *)

let rec lpi =
  let rec hlp k sign div = 
    LCons(k, function() ->  hlp (k +. (sign *. 4.0 /. div)) (-.sign) (div+.2.0) )
  in
  hlp 4.0 (-1.0) 3.0;;

ltake (4, (lpi ));;


let lmap_3 f l1 = 
  let rec hlp f = function
    (LNil, LNil, LNil) -> LNil
    | LCons(x, xf), LCons(y, yf), LCons(z, zf) -> 
      LCons (f x y z, function() -> hlp f ((xf()), (yf()), (zf())) )
  in hlp f (l1, (ltl l1), (ltl (ltl l1)));;

let euler = (fun x y z -> z -. ((y -. z) *. (y -. z)) /. (x -. (2.0 *. y) +. z ) );;

let fast_pi = lmap_3 euler lpi;;

(* LAZY *)

type 'a lazylist = LNil | LCons of 'a * 'a lazylist Lazy.t;;

let l_hd = function
  LNil -> failwith "lhd"
  | LCons (x, _) -> x;;

let l_tl = function
  LNil -> failwith "ltl"
  | LCons (_, lazy t) -> t;;

let rec l_from k = LCons (k, lazy (l_from (k+1)));;

let rec l_take = function
  (0, _) -> []
  | (_, LNil) -> []
  | (n, LCons(x,lazy xs)) -> x::l_take(n-1,xs);;

let l_pi = 
  let rec hlp k sign div = 
    let new_sign = sign *. (-1.0)
    in 
    let new_div = div +. 2.0
    in
    LCons(k, lazy (hlp (k +. (sign *. 4.0 /. div)) new_sign new_div) )
  in 
  hlp 4.0 (-1.0) 3.0;;

l_take (4, (l_pi ));;

let l_map_3 f l1 = 
  let rec hlp f = function 
    (LNil, LNil, LNil) -> LNil
    | LCons(x, lazy xf), LCons(y, lazy yf), LCons(z, lazy zf) -> 
      LCons (f x y z, lazy (hlp f (xf, yf, zf)))
  in hlp f (l1, (l_tl l1), (l_tl (l_tl l1)));;

let fast_l_pi = l_map_3 euler l_pi;;
