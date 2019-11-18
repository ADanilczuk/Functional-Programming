

type ('a, 'b) memo = 
	| Leaf 
	| Node of ('a * 'b * ((('a, 'b) memo) ref) * (('a, 'b) memo) ref);;


let create_empty : ('a -> 'b) -> (('a, 'b) memo ref) =
	fun _ -> ref Leaf;;


let rec find mem arg =
  match !mem with
  | Leaf -> None
  | Node (a, b, l, r) ->   if a = arg then Some b 
			   else
                             if arg < a then find l arg
	                     else
				find r arg;;


let rec insert mem a b =
  match !mem with
  | Leaf -> mem := Node(a, b, ref Leaf, ref Leaf)
  | Node (x, y, l, r) -> if a > x then	insert r a b
			else
		          insert l a b;;


let check_time f a =
  print_string "\n";
  let t = Sys.time () in
  let _ = f a in
  (Sys.time ()) -. t;;


let rec fib n =
  if n = 0 then	0
  else
    if n = 1 then 1
    else
      (fib (n-1)) + (fib (n-2));;

let how_many = 40;;
let wynik = fib how_many;;

print_float (check_time fib how_many);;


let memo_for_fib : (int, int) memo ref = ref Leaf;;

let rec fib_memo n=
  if n = 0 then	0
  else
    if n = 1 then 1
    else
      match find memo_for_fib n with
      | Some x -> x
      | None -> let wynik = (fib_memo (n-1)) + (fib_memo (n-2)) in
	        insert memo_for_fib n wynik;
                wynik;;

print_float (check_time fib_memo 32);;
assert ((fib_memo 30) = 832040);;

fib_memo 40;;
fib_memo 4;;
