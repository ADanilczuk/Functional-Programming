
let asssert = fun booll -> if booll then print_string "works\n" 
	else print_string "ERROR\n";;


let rec sufixy : 'a list -> 'a list list -> 'a list list = 
	fun lista actual -> match lista with
							[] -> actual@[[]]
						|  	_::tail -> sufixy tail (actual@[lista]);;

						
let sufix lista = sufixy lista [];;

asssert ((sufix [1;2;3]) = [[1;2;3];[2;3];[3];[]]);;
asssert ((sufix []) = [[]]);;
asssert ((sufix [1]) = [[1];[]]);;



let rec odwroc_liste lista = match lista with [] -> [] | a::b -> (odwroc_liste b)@[a];;

asssert ((odwroc_liste [1;2;3]) = [3;2;1]);;
asssert ((odwroc_liste []) = []);;
asssert ((odwroc_liste [1]) = [1]);;


let rec odwroc_kazda_liste lista = match lista with [] -> [] | a::b -> (odwroc_liste a)::(odwroc_kazda_liste b);;

let prefix lista = odwroc_kazda_liste(odwroc_liste (sufix (odwroc_liste lista)));;

asssert ((prefix [1;2;3]) = [[];[1];[1;2];[1;2;3]]);;
asssert ((prefix []) = [[]]);;
asssert ((prefix [1]) = [[];[1]]);;



