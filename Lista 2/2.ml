
let asssert = fun booll -> if booll then print_string "works\n" 
	else print_string "ERROR\n";;

let rec give_last_el lista = match lista with
							[] -> failwith ";o"
							| a::[] -> a
							| a::b -> give_last_el b;;

let rec remove_last lista = match lista with
							[] -> failwith ";o"
							| a::[] -> []
							| a::b -> [a]@(remove_last b);;

let move_single_el lista = [give_last_el lista]@(remove_last lista);;

let rec cycle lista ile = if ile = 0 
						then lista 
					  else 
					  	cycle (move_single_el lista) (ile-1);;

asssert ((cycle [1;2] 1) = [2;1]);
asssert ((cycle [1;2;3;4] 3) = [2;3;4;1]);
