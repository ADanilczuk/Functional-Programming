
let asssert = fun booll -> if booll then print_string "works\n" 
	else print_string "ERROR\n";;


let rec mergowanie_niefajne : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list = 
	fun cmp list1 list2 -> if list1 = [] then list2 
							else if list2 = [] then list1
								else let (el1, t1) = match list1 with [] -> failwith "" | a::t1 -> (a,t1) in
									let (el2, t2) = match list2 with [] -> failwith "" | b::t2 -> (b,t2) in
										if cmp el1 el2 then
											el2::(mergowanie_niefajne cmp list1 t2)
										else 
											el1::(mergowanie_niefajne cmp t1 list2);;


let merge_nie_fajny cmp list1 list2 = mergowanie_niefajne cmp list1 list2;;

asssert ((merge_nie_fajny (fun a b -> a>b) [1;4;7] [2;5;9]) = [1;2;4;5;7;9]);;
asssert ((merge_nie_fajny (fun a b -> a>b) [] []) = []);;
asssert ((merge_nie_fajny (fun a b -> a>b) [] [1]) = [1]);;




let rec mergowanie : ('a -> 'a -> bool) -> 'a list -> 'a list -> 'a list -> 'a list = 
	fun cmp list1 list2 actual -> if list1 = [] then actual@list2 
							else if list2 = [] then actual@list1
								else let (el1, t1) = match list1 with [] -> failwith "" | a::t1 -> (a,t1) in
									let (el2, t2) = match list2 with [] -> failwith "" | b::t2 -> (b,t2) in
										if cmp el1 el2 then
											mergowanie cmp list1 t2 (actual@[el2])
										else 
											mergowanie cmp t1 list2 (actual@[el1]);;

let merge cmp list1 list2 = mergowanie cmp list1 list2 [];;

asssert ((merge (fun a b -> a>b) [1;4;7] [2;5;9]) = [1;2;4;5;7;9]);;
asssert ((merge (fun a b -> a>b) [] []) = []);;
asssert ((merge (fun a b -> a>b) [] [1]) = [1]);;





let rec divide : 'a list -> 'a list * 'a list -> bool -> 'a list * 'a list = 
	fun lista actual right -> match lista with [] -> actual | a::b -> 
								if right then
									divide b (fst actual, (snd actual)@[a]) (not right)
								else
									divide b ((fst actual)@[a], snd actual) (not right);;



let rec mergesort : ('a -> 'a -> bool) -> 'a list -> 'a list =
	fun cmp lista -> match lista with [] -> [] | a::[] -> [a] | a::b ->
	let (l1, l2) = divide lista ([], []) true in 
		merge cmp (mergesort cmp l1) (mergesort cmp l2);;



asssert ((mergesort (fun a b -> a>b) [])=[]);;
asssert ((mergesort (fun a b -> a>b) [1])=[1]);;
asssert ((mergesort (fun a b -> a>b) [3;4;2;10;1])=[1;2;3;4;10]);;

let rec print_list = function lista -> match lista with
[] -> print_string ""
| e::l -> print_int e ; print_string "," ; print_list l;;

let rec print_list_list = function lista -> match lista with
[] -> print_string ""
| e::l -> print_string "["; print_list e ; print_string "]" ; print_list_list l;;


