(* -------- 1 ------*)

let rec add_element ls e=
  match ls with
   |[] -> []
   |a::tail -> [a @ [e]] @ (add_element tail e);;

let rec make_sub ls=
  match ls with
  | head::[] -> [ [head] ]
  | head::tail -> [[head]] @ (make_sub tail) @ (add_element (make_sub tail) head);;
  
let rec rmap f xs ys= 
  match xs with
  |[] -> ys
  |head::tail -> rmap xs (f x::ys);;
  
let sublists ls=
  if ls = [] then []
  else
    [[]] @  make_sub ls;;

sublists [];;
sublists ["a";"b";"c"];;


(*------- 2 ------*)

let rec last_el ls=
  match ls with
  |[] ->[]  
  |head::[] -> head
  |head::tail -> last_el tail;;

let rec list_without_last ls=
  match ls with
  | [] -> []
  |head::[] -> []
  |head::tail -> [head]@(list_without_last tail);;


let rec cycle ls n=
  if n=0 then ls
  else
    cycle ([last_el ls] @ (list_without_last ls)) (n-1);;


cycle [1;2;3;4] 3;;


(*-------- 3 --------*)

let rec merge cmp a b =
  if a= [] then b
  else if b=[] then a
  else
    match a with
    |h_a::tl_a -> match b with
                  |h_b::tl_b -> if (cmp h_a h_b) then h_a::(merge cmp tl_a b)
                                else h_b:: (merge cmp a tl_b);;
                              
      
(merge (fun a b -> a<b) [1;4;7] [2;5;9]);;
(merge (fun a b -> a>b) [6;1;0] [9;7;2]);;

let merge_rek cmp a b=
  merge_r cmp a b [[]];;

let rec merge_r cmp a b ls =
  if a= [] then b@ls
  else if b=[] then a@ls
  else
    match a with
    |[] -> []
    |h_a::tl_a -> match b with
                  |[] ->[]
                  |h_b::tl_b -> if (cmp h_a h_b) then (merge_r cmp tl_a b (h_a::ls))
                                else (merge_r cmp a tl_b (h_b::ls));;


(merge_rek (fun a b -> a<b) [1;4;6] [2;5;7]);;


let rec mergesort cmp l=
  match l with
  |[]| [_]-> l
  |_ -> let (fst, snd) = part l in
                       merge cmp (mergesort cmp fst) (mergesort cmp snd);;

let divide l=
  

(*------- 4 ------*)

let rec part cond ls tr fs =
  match ls with
  |[] -> (tr,fs)
  |head::tail -> if (cond head) then  (part cond tail (tr@head) fs)
                  else  (part cond tail tr (fs@[head]));;
     
let partition x ls=
  part x ls [] [];;


let rec quicksort cond ls=
  match ls with
  |[] -> []
  |head::[] -> [head]
  |head::tail ->   let (x,y)=  (partition  (cmp k) tail) in
                   (quicksort comp x)@(head::(quicksort comp y);;


                                     
(*------- 5 ------*)



(*------- 6 ------*)

let rec sufix_r ls acc=
    match ls with
    |[] -> [[]]@acc
    |head::tail -> (sufix_r tail (ls::acc));;

let rec sufixes ls=
  reverse (sufix_r ls []);;

sufixes [1;2;3];;

let rec reverse ls=
  match ls with
  |[] -> []
  |head::tail -> (reverse tail)@[head];;



let rec rev_all ls=
  match ls with
  |[]->[]
  |head::tail -> (reverse head)::(rev_all tail);;

let prefixes ls=
  rev_all (sufix_r (reverse ls) []);;

prefixes [1;2;3];;
