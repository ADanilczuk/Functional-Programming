
type 'a lnode = {item: 'a; mutable next: 'a lnode};;


let mk_circular_list e =
	let rec x = {item=e; next=x} in x;;

let insert_tail e l =
	let x = {item=e; next=l.next}
	in l.next <- x; x;;

let insert_head e l =
	let x = {item=e; next=l.next}
	in l.next <- x; l;;


let make_cycle_list n =
		let l = mk_circular_list 1
		in
			let i = ref n
			in while !i > 1 do
				insert_head !i l;
				i := !i - 1;
			done;
			l;;

              
let jozef n m = 
		if m < 1 then failwith "Error!";
		let x = ref [] in
      let rec usun_co ile_jeszcze poprz akt = 
          if ile_jeszcze = 0 then
          (
            if akt == akt.next then 
              x := (akt.item) :: (!x)
            else
            (
              x := (akt.item) :: (!x);
              poprz.next <- akt.next;
              usun_co (m-1) poprz akt.next 
            )
          )
          else 
            usun_co (ile_jeszcze - 1) akt (akt.next)
      in let k = make_cycle_list n in
        (usun_co (m-2) k k.next; List.rev !x);;
