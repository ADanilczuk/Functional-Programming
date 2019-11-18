let rec fix f = f (fix f);;

let fact n =
    let i = ref 1
    and res = ref 1
    in while !i <= n do
        res := !res * !i;
        i := !i + 1;
    done;
       !res;;

fact 3;;
fact 4;;

external magic: 'a -> 'b = "%identity"
                         
let fix' f x =
    let f' = ref (magic) in
    while !f' x != (f !f') x do
        f' := f !f'
    done;
    !f' x;;

#rectypes

let fix' f =
    let f' = ref (fun _ -> failwith "error") in
    let fix_aux f n = f (!f' f) n
    in f' := fix_aux;
    !f' f;;

let fact' n =
    let acc = ref (fun x -> x) in
    let fact_aux n =
        if n = 0
            then 1
            else n * (!acc (n-1))
    in acc := fact_aux;
       !acc n;;

fact' 4;;
