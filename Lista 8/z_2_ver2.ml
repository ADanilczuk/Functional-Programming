module type VERTEX =
sig
  type t
  type label
  val create_label : int -> label
  val equal : t -> t -> bool
  val create : label -> t
  val label : t -> label
end;;
 
module type EDGE =
sig  
  type vertex
  type t
  type label
  val create_label : int -> label
  val equal : t -> t -> bool
  val create : label -> vertex -> vertex -> t
  val get_beg : t -> vertex
  val get_end : t -> vertex
end;;

(*
module V : VERTEX  =
  struct
    type label = L of int
    type t = Vrtx of label
 
    let create_label a = L a
 
    let equal v1 v2 =
      match v1 with
      | v2 -> true
      | _ -> false
 
    let create l = Vrtx l
   
    let label v =
      match v with
     | Vrtx l -> l
  end;;

 module E : EDGE with type vertex = V.t  =
  struct
    type vertex = V.t
    type label = L of int
    type t = Edge of label * vertex * vertex
 
    let create_label a = L a
    let equal e1 e2 =
      match e1 with
      | e2 -> true
      | _ -> false
   
    let create l v1 v2=
      Edge (l, v1, v2)
   
    let label e =
      match e with
      | (l, v1, v2) -> l
   
    let get_beg e =
      match e with
      | Edge(l, v1, v2) -> v1
   
    let get_end e =
      match e with
      | Edge (l, v1, v2) -> v2
  end;;
    
 *)
  
 
module type GRAPH =
sig
(* typ reprezentacji grafu *)
  type t
  module V : VERTEX
  type vertex = V.t
  module E : EDGE with type vertex = vertex
  type edge = E.t
  (* funkcje wyszukiwania *)
  (*val get_labels : vertex list -> V.label list *)
  val mem_v : t -> vertex -> bool
  val mem_e : t -> edge -> bool
  val mem_e_v : t -> vertex -> vertex -> bool
  val find_e : t -> vertex -> vertex -> edge
  val succ : t -> vertex -> vertex list
  val pred : t -> vertex -> vertex list
  val succ_e : t -> vertex -> edge list
  val pred_e : t -> vertex -> edge list
  (* funkcje modyfikacji *)
  val empty : t
  val add_e : t -> edge -> t
  val add_v : t -> vertex -> t
  val rem_e : t -> edge -> t
  val rem_v : t -> vertex -> t
  (* iteratory *)
  val fold_v : ( vertex -> 'a -> 'a ) -> t -> 'a -> 'a
  val fold_e : ( edge -> 'a -> 'a ) -> t -> 'a -> 'a
end;;
 
module Graph :(*(V:VERTEX) (E:EDGE with type vertex = V.t):*) GRAPH
  (* with type vertex=V.t and type edge= E.t *)=

module Graph : GRAPH =
struct
  module V : VERTEX  =
  struct
    type label = L of int
    type t = Vrtx of label
 
    let create_label a = L a
 
    let equal v1 v2 =
      match v1 with
      | v2 -> true
      | _ -> false
 
    let create l = Vrtx l
   
    let label v =
      match v with
     | Vrtx l -> l
  end
  type vertex = V.t
  (*module E : EDGE with type vertex = vertex*)
  module E : EDGE with type vertex = V.t  =
  struct
    type vertex = V.t
    type label = L of int
    type t = Edge of label * vertex * vertex
 
    let create_label a = L a
    let equal e1 e2 =
      match e1 with
      | e2 -> true
      | _ -> false
   
    let create l v1 v2=
      Edge (l, v1, v2)
   
    let label e =
      match e with
      | (l, v1, v2) -> l
   
    let get_beg e =
      match e with
      | Edge(l, v1, v2) -> v1
   
    let get_end e =
      match e with
      | Edge (l, v1, v2) -> v2
  end
 
  type edge = E.t
  type t = Empty | Vrtx of vertex * t| Edg of edge * t
 
  let mem_v g v =
    let rec aux g =
      match g with
      | Empty -> false
      | Vrtx (x, gr) -> if V.equal v x = true then true else aux gr
      | Edg (x, gr) -> if E.get_beg x = v || E.get_end x = v then true else aux gr
    in aux g
 
  let mem_e g e =
    let rec aux g =
      match g with
      | Empty -> false
      | Vrtx (x, gr) -> aux gr
      | Edg (x, gr) -> if (E.equal x e) = true then true else aux gr
    in aux g
 
 let mem_e_v g v1 v2 = (* czy istnieje krawedz o wiercholkach v1 i v2*)
    let rec aux g =
      match g with
      | Empty -> false
      | Vrtx (x, gr) -> aux gr
      | Edg (x, gr) -> if (E.get_beg x) = v1 && (E.get_end x) = v2 then true else aux gr
    in aux g
 
  let find_e g v1 v2 =
    let rec aux g =
      match g with
      (*| Empty -> false*)
      | Vrtx (x, gr) -> aux gr
      | Edg (x, gr) -> if E.get_beg x = v1 && E.get_end x = v2 then x else aux gr
    in aux g
 
  let succ g v =
    let rec aux g acc=
      match g with
      | Empty -> acc
      | Vrtx (x, gr) -> aux gr acc
      | Edg (x, gr) -> if E.get_beg x = v then let v2 = E.get_end x in aux gr (v2::acc) else aux gr acc
    in aux g []
 
  let pred g v =
    let rec aux g acc=
      match g with
      | Empty -> acc
      | Vrtx (x, gr) -> aux gr acc
      | Edg (x, gr) -> if E.get_end x = v then let v2 = E.get_beg x in aux gr (v2::acc) else aux gr acc
    in aux g []
 
  let succ_e g v =
    let rec aux g acc =
      match g with
      | Empty -> acc
      | Vrtx (x, gr) -> aux gr acc
      | Edg (x, gr) -> if E.get_beg x = v then aux gr (x::acc) else aux gr acc
    in aux g []
 
  let pred_e g v =
    let rec aux g acc =
      match g with
      | Empty -> acc
      | Vrtx (x, gr) -> aux gr acc
      | Edg (x, gr) -> if E.get_end x = v then aux gr (x::acc) else aux gr acc
    in aux g []
 
  let empty = Empty
 
  let add_e g e = Edg (e,g)
 
  let add_v g v = Vrtx(v,g)
 
  let rem_e g e =
    let rec aux g =
      match g with
      | Empty -> Empty
      | Vrtx(v, gr) -> Vrtx(v, aux gr)
      | Edg (e1, gr) -> if E.equal e e1 then aux gr else Edg (e1, aux gr)
    in aux g
 
  let rem_v g v =
    let rec aux g =
      match g with
      | Empty -> Empty
      | Vrtx(v1, gr) -> if V.equal v1 v then aux gr else  Vrtx(v1, aux gr)
      | Edg (e1, gr) -> Edg (e1, aux gr)
    in aux g
 
    let fold_v f g en =
      let rec aux g acc done_vrtx =
        match g with
        | Empty -> acc
        | Vrtx (v1, gr) -> if List.mem v1 done_vrtx then  aux gr acc done_vrtx else aux gr (f v1 acc) (v1::done_vrtx)
        | Edg (e1, gr) -> let v1 = E.get_beg e1 in
                            let v2 = E.get_end e1 in
                              let acc1 = if List.mem v1 done_vrtx then acc else (f v1 acc) in
                                if List.mem v2 done_vrtx then (aux gr acc1 (v1::done_vrtx)) else aux gr (f v2 acc1) (v1::(v2::done_vrtx))
      in aux g en []
 
  let fold_e f g en =
    let rec aux g acc done_edgs =
      match g with
      | Empty -> acc
      | Vrtx (v1, gr) -> aux gr acc done_edgs
      | Edg (e1, gr) ->  if List.mem e1 done_edgs then (aux gr acc done_edgs) else aux gr (f e1 acc) (e1::done_edgs)
    in aux g en []
end;;


 
(*TESTY*)

 let example_g = Graph.(add_e empty (E.create (E.create_label 3) (V.create (V.create_label 2)) (V.create (V.create_label 5))));;
 
 let suc2 = Graph.(succ example_g (V.create (V.create_label 2)));;

 let pred2 = Graph.(pred example_g (V.create (V.create_label 5)));;
 let ex_g = Graph.empty;;
