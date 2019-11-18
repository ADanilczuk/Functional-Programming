(* -------- 1 ------*)

let rec add_element ls e=
  match ls with
    [] -> []
   |a::tail -> [a @ [e]] @ (add_element tail e);;


let rec make_sub ls=
  match ls with
  | [] -> failwith "pusto¶æ"
  | head::[] -> [ [head] ]
  | head::tail -> [[head]] @ (make_sub tail) @ (add_element (make_sub tail) head);;
  

  
let sublists ls=
  if ls = [] then []
  else
    [[]] @  make_sub(ls);;

sublists [];;
sublists ["a";"b";"c"];;


(*------- 2 ------*)

