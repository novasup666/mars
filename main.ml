open Printf
let () = Printexc.record_backtrace true


type math_exp =
| Sum of math_exp * math_exp
| Minus of math_exp * math_exp
| Dot of math_exp * math_exp
| Slash of math_exp * math_exp
| Fact of math_exp
| Value of float
(*rationnal numbers are stored as int*int *)
  

type stream = {
  str : string;
  mutable index : int
}

let new_stream str =
  {str = str; index = 0}

let rec bcd a b = 
  if a < b then bcd b a
  else 
    let r = b mod a in
    if r = 0 then b else bcd b r

let normalize (d,n) = 
  if n = 0 then failwith "div by 0 error"
  else (
    let q = bcd (abs d) (abs n) in
    if d*n <0 then
      (-d/q,n/q)
    else(d/q,n/q)
  )

exception SyntaxError
let peek s =
  if s.index < String.length s.str then Some s.str.[s.index]
  else None

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
  | _ -> raise SyntaxError


(*The functions are named phonetically according to the S and R state of the rules*)
let rec ess s =
  match peek s with
  | Some '(' -> (
    let l = arr s in
    match peek s with
    |Some '+' -> discard s; Sum (l,(ess s))
    |Some '*' -> discard s; Dot (l,(ess s))
    |Some '/' -> discard s; Slash (l,(ess s))
    |Some '-' -> discard s; Minus (l,(ess s))
    |Some '!' -> discard s; Fact l
    |Some c -> if is_number c then let n = the_number s in Value (float_of_string n) else error s
    |None -> l
  )
  |Some c -> if is_number c then let n = the_number s in Value (float_of_string n) else error s
  | None -> raise SyntaxError
and arr s  = 
  expect s '(';
  discard s;
  let e = ess s in
  expect s ')';
  discard s;
  e


let rec facto n  = n*.(facto (n-.1.))

let parse s = ess (new_stream s)

let rec eval e = 
  match e with
  | Sum (a,b) -> (eval a) +. (eval b)
  | Dot (a,b) -> (eval a) *. (eval b)
  | Minus(a,b) -> (eval a) -. (eval b)
  | Slash(a,b) -> (eval a) /. (eval b)
  | Fact a -> let n = eval a in if floor n = n then facto n else raise SyntaxError
  | Value a -> a