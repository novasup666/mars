open Printf
open Stack
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



(*The whole fun is here*)  
let shunting_yard s = 
  let p = Stack.create() in
  let rec inner s v  =
    match peek s with
    | Some '+'  -> discard s; push Plus p; inner s v 
    | Some '*'  -> discard s; push Dot p; inner s v 
    | Some '/'  -> discard s; push Slash p; inner s v
    | Some '-' ->  discard s; push 
    | Some c when is_number c -> let g = the_number s in inner s (g::v) op
    | Some ')' -> discard s; ([],[])
    | None -> ([],[])
    | Some _    -> error s
  in
  inner s []


let rec facto n  = 
  assert (n >=0);
  if n = 0 then 1. else  (float_of_int n) *.(facto (n-1))

let parse s = cons_p (new_stream s)

let rec eval e = 
  match e with
  | Sum (a,b) -> (eval a) +. (eval b)
  | Dot (a,b) -> (eval a) *. (eval b)
  | Minus(a,b) -> (eval a) -. (eval b)
  | Slash(a,b) -> (eval a) /. (eval b)
  | Fact a -> let n = eval a in if floor n = n then facto (int_of_float n) else raise SyntaxError
  | Value a -> a
  | Empty -> 0.
(*let evaluate s = eval (parse s)
*) 