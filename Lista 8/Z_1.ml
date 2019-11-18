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
  type 'a t = Empty | Push of (priority * 'a)* ('a t)
  exception EmptyPQueue
  let empty = Empty

  let insert pq p e = 
    let rec aux pq =
      (match pq with 
      | Empty -> Push((p,e),Empty)
      | Push ((px, ex),xs) -> 
          (match px with 
            | x when x > p ->Push( (x,ex),(aux xs))
            | x when x <= p -> Push((p,e),pq)))
    in aux pq
  
  let remove pq =
    match pq with 
    | Empty -> raise(EmptyPQueue)
    | Push((a,b),xs) -> (a,b,xs)

end;;

let test = PQueue.insert PQueue.empty 13 "jeej";;
PQueue.remove test;;
let test2 = PQueue.insert test 10 "ojej";;
PQueue.remove test2;;


(* 2 *)

let sort_queue ls =
  let rec insert_to_q ls acc =
    match ls with
    |[] -> acc
    |x::xs -> insert_to_q xs (PQueue.insert acc x " ") 
  in 
  let rec return_sorted pq =
    match pq with
    |x when x=PQueue.empty -> []
    |a -> let (prior, el, rest) = PQueue.remove pq in
          prior::(return_sorted rest)
  in
  return_sorted (insert_to_q ls PQueue.empty);;

  sort_queue [ 9 ; 5 ;13] ;;

  (* 3 *)


module type ORDTYPE =
sig
  type t
  type comparison = LT | EQ | GT
  val compare : t -> t -> comparison
end;;
          
module OrdType (PQ : PQUEUE) : ORDTYPE with type t = PQ.priority =
struct 
  type t = PQ.priority
  type comparison = LT | EQ | GT
  let compare s1 s2 = if s1<s2 then LT else
                      if s1>s2 then GT else EQ
end ;;


