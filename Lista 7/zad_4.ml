let (fresh, reset) =
    let counter = ref 1 in
    (fun s -> 
        let res = s ^ string_of_int !counter in incr counter;
        res),
    (fun n -> counter :=(1+ n));;


fresh "x";;
reset 5;;
