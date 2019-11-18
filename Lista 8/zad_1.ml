
module type PQUEUE =
sig
type priority
type 'a t
exception EmptyPQueue
val empty : 'a t
val insert : 'a t -> priority -> 'a -> 'a t
val remove : 'a t -> priority * 'a * 'a t
end;;
 
module PQueue : PQUEUE with type priority = int =
struct
  type priority = int
 
  type 'a t = Empty | Push of (priority * 'a) * ('a t)
 
  exception EmptyPQueue
 
  let empty = Empty
 
  let insert pq p e =
    let rec aux pq =
      (match pq with
      | Empty -> Push((p,e),Empty)
      | Push ((px, ex),(xs)) ->
          (match px with
            | x when x > p -> Push((x,ex),(aux xs))
            | x when x <= p -> Push((p,e),pq)))
    in aux pq
 
  let remove pq =
    match pq with
    | Empty -> raise(EmptyPQueue)
    | Push((a,b),xs) -> (a,b,xs)
 
end;;

 
let pq = PQueue.insert PQueue.empty 1000 "kot";;
 
let pq = PQueue.insert (PQueue.insert PQueue.empty 1000 "kot") 2 "pies";;
 
PQueue.remove pq;;

(* 2 *)
let sort_with_pqueue xs =
  let rec insert_elements xs acc =
    match xs with
    | [] -> acc
    | b::bs -> insert_elements bs (PQueue.insert acc b b)
  in
    let rec take_elements pq res =
      match pq with
      | x when x = PQueue.empty -> []
      | _ -> let (el, el2, pq2) =  PQueue.remove pq in take_elements pq2 res@[el]
    in take_elements (insert_elements xs PQueue.empty) [];;


(* 3 *)
module type ORDTYPE =
sig
  type t
  type comparison = LT | EQ | GT
  val compare : t -> t -> comparison
end
 
 
module OrdTypeInt : ORDTYPE   =
struct
  type t = int
  type comparison = LT | EQ | GT
  let compare a b =
    if a<b then LT else
    if a>b then GT else EQ
end
 
 
module FPQueue (OrdType:ORDTYPE) : PQUEUE with type priority = OrdType.t  =
struct
  type priority = OrdType.t
 
  type 'a t = Empty | Push of (priority * 'a) * ('a t)
 
  exception EmptyPQueue
 
  let empty = Empty
 
  let insert pq p e =
    let rec aux pq =
      (match pq with
      | Empty -> Push((p,e),Empty)
      | Push ((px, ex),(xs)) ->
          (match px with
            | x when (OrdType.compare px x) = OrdType.LT -> Push((x,ex),(aux xs))
            | _ -> Push((p,e),pq)))
    in aux pq
 
  let remove pq =
    match pq with
    | Empty -> raise(EmptyPQueue)
    | Push((a,b),xs) -> (a,b,xs)
 
end;;
 
module IntPQueue = FPQueue (OrdTypeInt)
 
let sort_with_fpqueue xs fpq=
  let rec insert_elements xs acc =
    match xs with
    | [] -> acc
    | b::bs -> insert_elements bs (IntPQueue.insert acc b b)
  in
    let rec take_elements pq res =
      match pq with
      | x when x = IntPQueue.empty -> []
      | _ -> let (el, el2, pq2) =  IntPQueue.remove pq in take_elements pq2 res@[el]
    in take_elements (insert_elements xs IntPQueue.empty) [];;


(* U¿ycie modu³ów pierwszego rodzaju *)
let sort (type s) (module Ordtype:ORDTYPE with type t =s) xs =
  let module F = Q(Ordtype) in
  ...
;;

