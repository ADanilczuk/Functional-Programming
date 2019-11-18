(*----- 3 -----*)

type 'a frame = Assumption of 'a logical_formula * 'a logical_formula list;;
 
type 'a deduction = FrameAndConclusion of 'a frame * 'a logical_formula;;


type 'a logical_formula = Var of char| Neg of 'a logical_formula | Con of 'a logical_formula * 'a logical_formula
                          | Dis of 'a logical_formula * 'a logical_formula | Imp of  'a logical_formula * 'a logical_formula;;

type 'a frame = 'a logical_formula * 'a deduction 
and 'a proof = Form of 'a logical_formula
             | P_Frame of 'a frame * 'a proof
             | P_Form of 'a logical_formula * 'a proof;;


type 'a tproof = |Assumption of 'a logical_formula
                 |ConI of 'a tproof * 'a tproof
                 |ConE1 of 'a proof
                 |ConE2 of 'a proof
                 |ImpI of 'a frame
                 |ImpE of 'a tproof * 'a tproof
                 |DisjI1 of 'a proof * 'a frame 
and 'a tframe = 'a logical_formula * 'a tproof;;
                 

(* Frame_Conclusion *)

(*----- 4 ------*)


let rec remove_duplicates xs acc=
  match xs with
  |[] -> acc
  |hd::tl -> if List.mem hd acc then remove_duplicates tl acc
             else remove_duplicates tl (hd :: acc);;

remove_duplicates ['p';'q';'s';'p'] [];;

let pos_neg formula =
  let rec hlp pos neg formula =
    match formula with
    | Var (x) -> (x::pos, neg)
    | Con (a, b) -> let (pos_a,neg_a) = hlp pos neg a in
                     let (pos_b, neg_b) = hlp pos neg b in (pos_a@pos_b, neg_a@neg_b)
    | Dis (a, b) -> let (pos_a,neg_a) = hlp pos neg a in
                    let (pos_b, neg_b) = hlp pos neg b in (pos_a@pos_b, neg_a@neg_b)
    | Imp(a, b) -> let (pos_b, neg_b) = hlp pos neg b in
                  let (pos_a, neg_a) =  hlp pos neg a in (pos_b@neg_a , neg_b@pos_a)
  in
  let pair = (hlp [] [] formula) in
  ( remove_duplicates (fst pair) [],
    remove_duplicates (snd pair) []);;


pos_neg (Con (Var('a'), Var('b')));;
pos_neg(Imp (Var('z'), Var('q')));;
pos_neg (Con(Var('q') , Imp (Var('z'), Var('q'))));;

pos_neg (Con (Var('p') , Con(Var('q') , Imp (Var('z'), Var('q')))));;


let rec pos_neg_proof_list list pos neg =
  match list with
  |[] -> (pos,neg)
  |hd::tl -> let (hd_pos, hd_neg) = pos_neg_proof hd pos neg in
             pos_neg_proof_list tl hd_pos hd_neg;;

let rec pos_neg_proof form pos neg =
  match form with
  |Var(a) -> (a::pos, neg)
  |Con(a,b)| Dis(a,b) -> let (pos_a, neg_a) = (pos_neg_proof a pos neg) in
                         pos_neg_proof b pos_a neg_a
                                                                             
  |Imp(a,b) -> let (pos_a, neg_a) = (pos_neg_proof a pos neg) in
               pos_neg_proof b neg_a pos_a
  |Assumption (a,b) ->  (let (pos_a, neg_a) = (pos_neg_prof a pos neg) in
                         let wylicz_liste list pos neg =
                          let rec pos_neg_proof_list list pos neg =
                          match list with
                          |[] -> (pos,neg)
                          |hd::tl -> let (hd_pos, hd_neg) = pos_neg_proof hd pos neg in
                                     pos_neg_proof_list tl hd_pos hd_neg
                          in
                           wylicz_liste b neg_a pos_a
   
  | (a,b) -> let (pos_a, neg_a) = (pos_neg_proof a pos neg) in
                  pos_neg_proof b pos_a neg_a;;

