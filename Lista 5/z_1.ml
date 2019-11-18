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

let rec (@$) ll1 ll2 =
  match ll1 with
  LNil -> ll2
  | LCons(x, xf) -> LCons(x, function() -> (xf()) @$ ll2);;

let rec lmap f = function
  LNil -> LNil
  | LCons(x, xf) -> LCons(f x, function() -> lmap f (xf()) );;
  
let sqr_llist = lmap (fun x -> x*x);;

 ltake (6, sqr_llist (lfrom 3));;

 ltake( 6, lfrom 3);;

 let rec lfilter pred = function
  LNil -> LNil
  | LCons(x, xf) -> if pred x
                    then LCons (x, function() -> lfilter pred (xf()))
                    else lfilter pred (xf());;

let rec liter f x = LCons(x, function() -> liter f (f x));;

lfilter (fun x -> (x mod 2) = 0) (toLazyList (ltake (11, (lfrom 7))));;

let np = lfilter (fun x -> (x mod 2) = 1) (lfrom 1);;


(* Zadanie 1 *)

let rec lpi =
  let rec hlp k sign div = 
    LCons(k, function() ->  hlp (k +. (sign *. 4.0 /. div)) (-.sign) (div+.2.0) )
  in
  hlp 4.0 (-1.0) 3.0;;

ltake (4, (lpi ));;

let fabs = fun x -> if x<0.0 then (-1.0 *. x) else x;;
(* lfilter (fun x -> (fabs (3.141592 -. x)) < 0.0001) pi;; *)

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

let l_map_3 f l1 = 
  let rec hlp f = function 
    (LNil, LNil, LNil) -> LNil
    | LCons(x, lazy xf), LCons(y, lazy yf), LCons(z, lazy zf) -> 
      LCons (f x y z, lazy (hlp f (xf, yf, zf)))
  in hlp f (l1, (l_tl l1), (l_tl (l_tl l1)));;

let fast_l_pi = l_map_3 euler l_pi;;
