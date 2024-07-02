open Printf

let () = Printexc.record_backtrace true


type math_exp =
| Plus 
| Minus 
| Dot 
| Slash 
| Value of float
| Empty
  

type stream = {
  str : string;
  mutable index : int
}

let new_stream str =
  {str = str; index = 0}

exception SyntaxError

let discard s = 
  if s.index >= String.length s.str then (printf "Unexpected end of input\n";  raise SyntaxError) else (s.index <- s.index +1)

let rec peek s =
  let inner s=
    if s.index < String.length s.str then Some s.str.[s.index]
    else None
  in
  match inner s with
  | None -> None
  | Some c -> if c = ' ' then (discard s; peek s) else Some c

let error s =
  match peek s with
  | None ->
    printf "Unexpected end of input\n";
    raise SyntaxError
  | Some c ->
    printf "Unexpected  %c at %d\n" c s.index;
    raise SyntaxError

let expect s c =
  let ac_option = peek s in
  match ac_option with
  | Some ac when ac = c -> s.index <- s.index + 1
  | _ -> error s

let discard s = 
  if s.index >= String.length s.str then (printf "Unexpected end of input\n";  raise SyntaxError) else (s.index <- s.index +1)

let is_numeric c = 
  let ascii_of_c = int_of_char c in
  ascii_of_c >=48 && ascii_of_c<=57 || c = '.'

let is_number c = 
  let ascii_of_c = int_of_char c in
  ascii_of_c >=48 && ascii_of_c<=57 
let rec the_number s = 
  match peek s with
  | Some c -> if is_numeric c then (discard s; (Char.escaped c)^(the_number s)) else ""
  | None -> ""

let pol_calc q = 
  let p = Stack.create () in
  while not (Queue.is_empty q) do
    let token = Queue.pop q in
    match token with
    | Plus -> let a = Stack.pop p in let b = Stack.pop p in Stack.push (a+.b) p
    | Dot -> let a = Stack.pop p in let b = Stack.pop p in Stack.push (a*.b) p
    | Slash -> let a = Stack.pop p in let b = Stack.pop p in Stack.push (b/.a) p
    | Minus -> let a = Stack.pop p in let b = Stack.pop p in Stack.push (b-.a) p
    | Value a -> Stack.push a p
    | Empty -> failwith "c vid"
  done;
  Stack.pop p

let op_prio exp = 
  match exp with
  | Plus -> 1
  | Minus -> 0
  | Dot | Slash -> 2
  | Value _ | Empty -> failwith "ce ne sont pas des opérateurs"

let op_push exp p q = 
  let op =   ref (Stack.pop p) in
  while (op_prio !op) > (op_prio exp) do
    Queue.push !op q;
    op := Stack.pop p 
  done;
  Queue.push exp q

(*The whole fun is here*)  
let rec shunting_yard s = 
  let p = Stack.create () in 
  let q = Queue.create () in
  let rec inner s  =
    match peek s with
    | Some '+'  -> discard s; op_push Plus p q; inner s 
    | Some '*'  -> discard s; op_push Dot p q; inner s  
    | Some '/'  -> discard s; op_push Slash p q; inner s 
    | Some '-' ->  discard s; op_push Minus p q; inner s 
    | Some c when is_number c -> let g = the_number s in Queue.push (Value (float_of_string g)) q; inner s 
    | Some ')' -> discard s;
    | Some '(' -> discard s; Queue.push (Value (shunting_yard s)) q
    | None -> ()
    | Some _    -> error s
  in
  inner s;
  pol_calc q



let parse s = shunting_yard (new_stream s)

