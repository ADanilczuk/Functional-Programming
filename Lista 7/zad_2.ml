

type 'a list_mutable = LMnil | LMcons of 'a * 'a list_mutable ref;;

let rec print_mut list =
  match list with
  | LMnil -> ()
  | LMcons(a, b) -> print_string a; print_string " "; print_mut !b;;

let rec porownaj list m_list  =
	match list, m_list with
	| ([], LMnil) -> true
	| ((a::b), LMcons (h, t)) -> a = h && porownaj b (!t)
	| _ -> false;;

let rec wstaw_na m_list n el =
    if n = 0 then m_list := LMcons(el, ref (!m_list))
	else
	match !m_list with
	| LMnil -> failwith "nie ma tylu elementow"
	| LMcons(a, b) -> wstaw_na b (n-1) el;;


let rec concat_copy m_list1 m_list_2 =
  match m_list1 with
  | LMnil -> m_list_2
  | LMcons (h, t) -> LMcons(h, ref(concat_copy (!t) m_list_2));;



let lista1 : 'a list_mutable = LMcons("aa", ref (LMcons("bb", ref LMnil)));;

print_mut lista1;;
let lista2 : 'a list_mutable = LMcons("11", ref (LMcons("22", ref LMnil)));;
let skonkat : 'a list_mutable = concat_copy lista1 lista2;;




porownaj ["aa";"bb";"11";"22"] skonkat;;

wstaw_na (ref skonkat) 4 "cc";;
wstaw_na (ref skonkat) 1 "ab";;

print_mut skonkat;;
porownaj ["aa";"bb";"11";"22";"cc"] skonkat;;
porownaj ["aa";"bb"] lista1;; (*lista1 skopiowana wiec nieruszona*)
porownaj ["11";"22";"cc"] lista2;; (*lista 2 zostala zywcem wzieta*)*)


let concat_share m_list1 m_list2 =
      match !m_list1 with
      | LMnil -> m_list2
      | _ ->  let rec aux  m_l_1 m_l_2 =
			match !m_l_1 with
     		        | LMnil -> failwith "coo"
			| LMcons(a, b) ->
				if (!b) = LMnil then b := !m_l_2
				else aux b m_l_2
	      in aux m_list1  m_list2; m_list1;;


let skonkat : 'a list_mutable ref = concat_share (ref lista1) (ref lista2);;

porownaj ["aa";"bb";"11";"22";"cc"] !skonkat ;;(*razem z ccc po poprzedni podpunkt zmodyfikowal jedna z list*)
wstaw_na skonkat 4 "ww";;
print_mut (!skonkat);;
porownaj ["aa";"bb";"11";"22";"cc";"ww"] !skonkat;;

porownaj ["aa";"bb";"11";"22";"ww";"cc"] lista1;; (*lista1 to w zasadzie nasza lista*)
porownaj ["11";"22";"ww";"cc"] lista2;; (*lista 2 jest jej czynnym skladnikiem*)

