
 (*------ 3 -------*)

 # type 'a mtree = MN of 'a * 'a forest  and 'a forest = EF | FO of 'a mtree * 'a forest;;

 let forest_preorder  mnode =
   let rec preo mnode acc =
   match mnode with
   | EF -> acc
   | FO (a , b) -> match a with
                   |MN (x, y) -> preo b (preo  y (x::acc))
 in List.rev(preo mnode []);;   


   let np = FO (MN (3,
                 FO (MN (2,
                         FO (MN (1,
                                 EF),
                             FO (MN (4,
                                     EF),
                                 EF))),
                       FO (MN (6,
                               EF),
                           FO (MN (7,
                                   EF),
                               EF)))),
                EF);;
                         


   forest_preorder np;;

   let forest_birthorder  mnode =
     let rec birtho mnode acc =
       match mnode with
       |EF -> acc
       |FO (a , b) -> match a with
                      |MN (x, y) -> birtho y (birtho b (x::acc) )
   in List.rev(birtho mnode []);;

   forest_birthorder np;;


   

   (*-------- 4 --------*)


   
   type 'a logical_formula = Var of char| Neg of 'a logical_formula | Con of 'a logical_formula * 'a logical_formula |
                             Dis of 'a logical_formula * 'a logical_formula;;

   let rec if_taut  xs values =
     match xs with
     |Var (a) -> find_value a values
     |Neg (a) -> if ((if_taut a values) =0 ) then 1
                 else 0
      |Dis (a,b) -> let ad= (if_taut a values) in
                    let bd = (if_taut b values) in
                    ( match (ad,bd) with
                      |(0,0) -> 0
                      |(1,0) | (0,1)| (1,1) -> 1)
     |Con (a,b) -> let ac = (if_taut a values) in
                   let bc = (if_taut b values) in
                    ( match (ac,bc) with
                     |(1,1) -> 1
                     |(0,1) | (1,0) | (0,0) -> 0);;
    
   
   
   let rec find_value x values =
     match values with
     |[] -> 9
     |a::b -> if (fst a) = x then (snd a)
              else
               find_value x b;;

   let rec find_vars xs acc =
     match xs with
     |Neg (a) -> find_vars a acc
     |Con (a,b) -> find_vars a (find_vars b acc)
     |Dis (a,b) -> find_vars a (find_vars b acc )
     |Var (a) -> a::acc;;

   let cons_uniq xs x = if List.mem x xs then xs else x :: xs

   let remove_duplicates xs = List.rev (List.fold_left cons_uniq [] xs);;
   
  let generate_values vars =
    let rec aux acc vars n = 
    match vars with 
    | [] -> acc
    | y::ys -> aux ((List.map (fun x -> (y,1)::x) acc)@(List.map (fun x-> (y,0)::x) acc)) ys n
    in aux [[]] vars (List.length vars);;


  let check_if_tautology xs=
    let vars = remove_duplicates (find_vars xs []) in
    let values = generate_values (vars) in

    let rec checker lists_of_values bollean =
        match lists_of_values with
        |[] -> true
        |hd::tl -> if (if_taut xs hd = 1) then checker tl true
                   else false
    in
    checker values true;;


  let t1 = Dis( Neg ( Con ( Var 'p',
                            Var 'q')),
                (Var 'p'));;


  let t2 = Dis ( Neg (Con (Con ( Dis ((Neg (Var 'p')),
                                      Var 'q'),
                                 Dis ((Neg (Var 'q')),
                                      Var 'r')),
                           (Dis (Var 'p', Var 'q')))),
                 (Var 'r'));;
                               

  check_if_tautology t1;;
  check_if_tautology t2;;

  (*  b  *)


  let rec nnf  xs =
    match xs with
    |Neg (a) -> ( match a with
                 |Con (x,y) -> (Dis ((nnf (Neg (x))),(nnf (Neg (y)))))
                 |Dis (x,y) -> (Con ((nnf (Neg (x))),(nnf (Neg (y)))))
                 |Neg (x) -> nnf x
                 |Var (x) ->Neg( Var (x)))
    |Con (a,b) -> Con ( nnf a, nnf b)
    |Dis (a,b) -> Dis ( nnf a, nnf b)
    |Var (a) -> Var (a);;
                 
                              
  nnf t1;;
 nnf t2;;

  (* c *)

let rec cnf_help a b =
  match (a,b) with 
  | (p, Con(q,r)) -> Con((cnf_help p q), (cnf_help p r))
  | (Con(q,r), p) -> Con((cnf_help q p), (cnf_help r p))
  | (p, q) -> Dis (p, q);;

let rec cnf xs = 
  match xs with
  | Var(a) -> Var(a)
            
  | Neg(Neg(a)) -> cnf a 
  | Neg(Con(a,b))  -> Dis( cnf (Neg(a)), cnf (Neg (b))) 
  | Neg(Dis(a,b))  ->  Con( cnf (Neg (a)), cnf (Neg (b)))
  | Neg(Var(a)) -> Neg(Var(a))
                 
  | Con(a,b) ->  Con(cnf a, cnf b)
  | Dis(a,b) -> cnf_help (cnf a) (cnf b);;
                                
cnf t1;; 
cnf t2;;

(*  e  *)

let rec dnf_help a b =
  match (a,b) with 
  | (p, Dis(q,r)) -> Dis((dnf_help p q), (dnf_help p r))
  | (Con(q,r), p) -> Con((dnf_help q p), (dnf_help r p))
  | (p, q) -> Con(p, q);;

let rec dnf xs = 
  match xs with
  | Var(a) -> Var(a)
           
  | Neg(Neg(a)) -> dnf a 
  | Neg(Con(a,b))  ->  Dis( dnf (Neg(a)), dnf (Neg (b))) 
  | Neg(Dis(a,b))  ->  Con( dnf (Neg (a)), dnf (Neg (b)))
  | Neg(Var(a)) -> Neg(Var(a))
                 
  | Dis(a,b) ->  Dis(dnf a, dnf b)
  | Con(a,b) -> dnf_help (dnf a) (dnf b);;
                                
dnf t2;;
cnf t2;;   
dnf t1;;
cnf t1;;
t1;;
