fun x->x;;

fun  x ->  x+2;;

fun  x -> List.hd [];;

let abc f g x = f (g (x));;


let rec element_ciagu x =
	if x=0 then 0
		else 
		1+ (2* element_ciagu (x-1))
	;;
	
let rec element_ciagu_rek x acc =
	if x=0 then acc
		else 
		element_ciagu_rek (x-1) (2*acc + 1)
	;;
	
let rec element_rek x =
	element_ciagu_rek x 0
;;


element_rek 30;;
element_ciagu 30;;







let zlozenie f g x = f (g (x));;

let rec it_zlozenie f n =
  if n=0 then fun x->x
    else zlozenie f (it_zlozenie f (n-1));;
	
let multiply  a b =
 repeat a (fun x -> (+) b x) 0;;
	 ;;

let (^)  a b =
  repeat (b-1) (fun x-> (^) a x) a
    ;;




let rec s x=
  if x=0 then 2
  else 2* (s (x-1));;
  

let hd s =  s 0;;

let t_s s= function x -> s (x+1);;

let add s c = function x -> (s x) +c;;

let map f s = function x-> f (s x);;

let map2 f s1 s2 = function x-> f (s1 x) (s2 x);;

let replace s n a =
  function x->
    (if x mod n = 0 then a
     else (s x);;

let take n s =  function x -> s (x*n + n);;

let rec scan f a s n =
            if n=0 then (f a (s 0))
            else f (scan f a s (n-1)) (s n);; 

scan (fun a b-> a+b) 2 s 2;;

let rec tabulate ?(x=0) n s=
  if x=n then []
  else (s x) :: (tabulate ~x:(x+1) n s);; 

tabulate 4 s;;



let ctrue: 'a->'a->'a = fun x y -> x;; 
let cfalse: 'a->'a->'a = fun x y-> y;;

let cand w1 w2 t f =
 w1 (w2 t f) f;;


let cor w1 w2 t f =
 w1 t (w2 t f);;

let cbool_of_bool b=
  if b then ctrue
  else cfalse;;

let bool_of_cbool =
  function b -> b true false;;
