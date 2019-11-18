
let asssert = fun booll -> if booll then print_string "works\n" 
	else print_string "ERROR\n";;



let rec add_to_everyone :'a list list -> 'a -> 'a list list =
	fun lista el -> match lista with
					[] -> []
				|   a::b -> [[el]@a]@(add_to_everyone b el);;


let rec wstaw_w_list : 'a list -> 'a -> 'a list list =
	fun lista el -> match lista with
					 [] -> [[el]]
					| a::b -> [[el]@[a]@b]@(add_to_everyone (wstaw_w_list b el) a);;


let rec wstaw_pomiedzy : 'a list list -> 'a -> 'a list list =
	fun lista el -> match lista with
						[] -> []
					|   a::b -> (wstaw_w_list a el)@(wstaw_pomiedzy b el);;


let rec all_perms : 'a list -> 'a list list =
	fun lista -> match lista with
		[] -> []
	  | a::[] -> [[a]]
	  | a::tail -> let per = all_perms tail in
	  	wstaw_pomiedzy per a ;;


asssert ((all_perms [1;2]) = [[1;2];[2;1]]);;
asssert ((all_perms []) = []);;
asssert ((all_perms [1;2;3]) = [[1;2;3];[2;1;3];[2;3;1];[1;3;2];[3;1;2];[3;2;1]]);;







