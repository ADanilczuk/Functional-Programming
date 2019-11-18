(*  Alicja Danilczuk  *)
(*       Lista 3      *)


(*-------- 1 --------*)

let rec oblicz_int_a xs acc =
      if xs=[] then acc
      else
        oblicz_int_a (List.tl xs) ( acc*10 + ((int_of_char (List.hd xs)) -48));; 


oblicz_int_a ['1';'2';'3'] 0;;


let oblicz_int_b xs =
  let wyliczenie acc x =
    acc*10 + ((int_of_char x)-48) in
  if xs=[] then 0
  else
    List.fold_left wyliczenie 0 xs;;

oblicz_int_b ['1';'2';'3'];;


(*----- 2 --------*)

let rec horner_a xs x acc=
  match xs with
  |[] -> acc
  |[a] -> acc+a
  |hd::tl ->
    horner_a tl x (x*(acc +hd));;

horner_a [3;0;7;2] 4 0;;

  
let horner_b xs x=
  let operacja_horner acc c=
    x*(acc+c) in
  (List.fold_left operacja_horner 0 xs)/x;;

horner_b [3;0;7;2] 4 ;;
horner_b [0] 2;;




(*-------- 3 ---------*)


let right_tl_a op xs=
  let rec right_tl xs acc=
     match xs with
     | [] -> List.rev(acc)
     | x::xs -> right_tl xs ((op x)::acc)
  in  
  right_tl xs [];;

right_tl_a (fun n -> n+1) [1;2;3];;


let rec right_tl_b op xs =
  let operacja_fold acc z=
    (op z)::acc  in
 List.rev( List.fold_left operacja_fold [] xs) ;;

right_tl_b (fun n -> n+1) [1;2;3];;


(*---------- 5 --------*)

let rec polynomial_rek xs x =
  let a = x in
  let rec pol xs x =
  match xs with
       |[] -> 0
       |hd::tl -> hd*x + (pol tl (x*a))
  in
  pol xs 1;;

polynomial_rek [2;-1;0;1] 4;;
polynomial_rek [2;7;0;3] 4;;

let rec polynomial_tl_rek xs x =
  let a = x in
  let rec pol_rek xs x acc =
    match xs with
    |[] -> acc
    |hd::tl ->  pol_rek tl (x*a) (acc+ (hd*x))
  in
  pol_rek xs 1 0;;

polynomial_tl_rek [2;-1;0;1] 4;;
polynomial_tl_rek [2;7;0;3] 4;;


let rec polynomial_right_rek xs x =
  let a = x in
  List.fold_right (fun z acc -> (z + acc * a))  xs 0;;

polynomial_right_rek [2;-1;0;1] 4;;
polynomial_right_rek [2;7;0;3] 4;;

let rec polynomial_left_rek xs x=
  let a = x in
  let operacja_pol z acc =
    (acc + z)*a
  in
  (List.fold_left operacja_pol 0 (List.rev xs))/x;;

polynomial_left_rek [2;-1;0;1] 4;;
polynomial_left_rek [2;7;0;3] 4;;

(*-------- 6 --------*)

let wstaw el xs=
  let rec wstawianie el xs ys acc =
    match xs with
    |[] -> (ys@[el])::acc
    |hd::tl -> wstawianie el tl (hd::ys) ((ys@(el::xs))::acc) 
  in
  wstawianie el xs [] [];;

let powstawiaj el xs =
  let rec powst el xs acc =
    match xs with
    |[] -> acc
    |hd::tl -> powst el tl ((wstaw el hd)@acc)
  in
  powst el xs [];;

let perms_right xs =
  match xs with
       |[] -> [[]]
       |hd::tl -> List.fold_right (fun x acc -> powstawiaj x acc) tl [[hd]];;

perms_right [1;2;3];;


let perms_left xs =
  match xs with
  |[] -> [[]]
  |hd::tl -> List.fold_left (fun acc x -> powstawiaj x acc) [[hd]] tl;;

perms_left [1;2;3];;



(*---------- 8 ----------*)

exception Mtx of string;;
 
let mtx_dim matrix =
  match matrix with
    |[] -> raise (Mtx "B³±d, macierz zerowego rozmiaru")
    |hd::tl ->  match hd with
               |[] -> raise (Mtx "B³±d, wiersz zerowego rozmiaru")
               |hd2::tl2 ->
                 let wynik =
                   let operacja acc x =
                   if not (snd acc) then  (0, false)
                    else
                      (fst acc, List.length x = fst acc)
                   in
                   List.fold_left operacja (List.length hd, true) matrix
                 in
                if not (snd wynik) then  raise (Mtx "D³ugo¶æ wierszy ró¿ni siê")
                else
                    ((fst wynik) ,List.length matrix);;

mtx_dim [[1;2];[3;4];[5;6]];;
mtx_dim [[]];;
mtx_dim [];;
mtx_dim [[1;2];[3]];;


(*----------- 9 -------------*)

let rec mtx_row matrix x =
    match matrix with
    |[] -> raise (Mtx "Macierz zbyt krótka")
    |hd::tl ->
      if x = 1 then hd
      else
        mtx_row tl (x-1);;

mtx_row [[1;2];[3;4]] 2;;


let mtx_column matrix x =
  let rec columns matrix x acc =
    match matrix with
    |[] -> acc
    |hd::tl -> if (List.length hd > (x-1)) then
                 columns tl x (acc@ [(List.nth hd (x-1))])
               else raise (Mtx "Zbyt krótkie wiersze")
  in
  columns matrix x [];;
  
mtx_column [[1;2];[3;4]] 2;;


let mtx_elem matrix x y =
  let row = mtx_row matrix x
  in
  if (List.length row > (y-1)) then List.nth row (y-1)
  else
    raise (Mtx "zbyt krótkie wiesze");;
     

mtx_elem [[1;2];[3;4];[5;6]] 3 1;;
mtx_elem [[1;2];[3;4];[5;6]] 3 3;;



(*-------- 10 ---------*)

let transpose matrix =
  let columns = snd (mtx_dim matrix) in
   let rec trans acc n =
    if n = (columns + 1) then acc
    else
      trans ((mtx_column matrix n)::acc) (n + 1)
    in List.rev(trans [] 1);;

transpose [[1;2];[3;4]];;


(*----------- 11 --------------*)

let rec rows_add xs ys =
  match xs with
  |[] ->[]
  |hd::tl ->
  (hd+(List.hd ys))::(rows_add tl (List.tl ys));;

let mtx_add m1 m2 =
  let rec add m1 m2 acc =
    match m1 with
    |[] -> acc
    |hd::tl -> 
           add (List.tl m1) (List.tl m2) ((rows_add (List.hd m1) (List.hd m2))::acc)
  in
 (List.rev ( add m1 m2 []));;

mtx_add [[1;2];[2;3]] [[1;1];[1;1]];;


(*----------- 12 -----------*)


let rec scalar_prod w1 w2 =
  match w1 with
  |[] -> if not (w2=[]) then raise (Mtx "Ró¿na ilo¶æ wspó³rzêdnych 2")
         else 0
  |hd::tl -> match w2 with
             |[] -> raise (Mtx "Ró¿na ilo¶æ wspó³rzêdnych 1")
             |hd2 :: tl2 -> (hd* hd2) + (scalar_prod tl tl2);;
   
                                        
scalar_prod [1;2] [3;4];;

let polynomial_scal w1 x =
  let a = x in
  let rec stworz_wielomian xs x dl=
    if (List.length xs < dl) then
      stworz_wielomian (xs@[x]) (x*a) dl
    else xs
  in
  (scalar_prod (List.rev w1) (stworz_wielomian [] x (List.length w1)))/x;;

 polynomial_scal [3;0;7;2] 4;;


 (*--------- 13 --------*)

 let mtx_apply matrix xs =
   List.map (fun ys -> scalar_prod ys xs) matrix;;

 mtx_apply [[-2;-1;2];[3;0;1];[2;2;-1]] [-2;-3;1];;


