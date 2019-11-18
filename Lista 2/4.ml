
let asssert = fun booll -> if booll then print_string "works\n" 
	else print_string "ERROR\n";;

let rec part_help: ('a -> bool) -> 'a list -> 'a list * 'a list -> 'a list * 'a list = 
	fun pred lista actual -> match lista with
						[] -> actual
					  | a::b -> if pred a then
					  				part_help pred b ((fst actual)@[a], snd actual)
					  			else part_help pred b (fst actual, (snd actual)@[a]);;

let partition : ('a -> bool) -> 'a list -> 'a list * 'a list = 
	fun pred lista -> part_help pred lista ([], []);;

asssert ((partition (function a -> a = 0 || a = 1) [1;2;0;3])=([1;0],[2;3]));;
asssert ((partition (function a -> a = 0 || a = 1) [1])=([1],[]));;


let rec quicksort : ('a -> 'a -> bool) -> 'a list -> 'a list = 
	fun pred lista -> match lista with [] -> [] | h::[] -> [h] | h::tail ->  
	let chosen_one = h in
	  	let one_arg_pred : 'a -> bool = fun el_to_compare -> pred chosen_one el_to_compare in
	  		let tuple_of_divided = partition one_arg_pred tail in 
	  			(quicksort pred (fst tuple_of_divided))@[h]@(quicksort pred (snd tuple_of_divided));;


 
asssert ((quicksort (fun a b -> a>b) [3;4;2;10;1])=[1;2;3;4;10]);;
asssert ((quicksort (fun a b -> a>b) [])=[]);;
asssert ((quicksort (fun a b -> a>b) [1])=[1]);;