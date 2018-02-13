exception MLFailure of string

type binop = 
  Plus 
| Minus 
| Mul 
| Div 
| Eq 
| Ne 
| Lt 
| Le 
| And 
| Or          
| Cons

type expr =   
  Const of int 
| True   
| False      
| NilExpr
| Var of string    
| Bin of expr * binop * expr 
| If  of expr * expr * expr
| Let of string * expr * expr 
| App of expr * expr 
| Fun of string * expr    
| Letrec of string * expr * expr
	
type value =  
  Int of int		
| Bool of bool          
| Closure of env * string option * string * expr 
| Nil                    
| Pair of value * value     

and env = (string * value) list

let binopToString op = 
  match op with
      Plus -> "+" 
    | Minus -> "-" 
    | Mul -> "*" 
    | Div -> "/"
    | Eq -> "="
    | Ne -> "!="
    | Lt -> "<"
    | Le -> "<="
    | And -> "&&"
    | Or -> "||"
    | Cons -> "::"

let rec valueToString v = 
  match v with 
    Int i -> 
      Printf.sprintf "%d" i
  | Bool b -> 
      Printf.sprintf "%b" b
  | Closure (evn,fo,x,e) -> 
      let fs = match fo with None -> "Anon" | Some fs -> fs in
      Printf.sprintf "{%s,%s,%s,%s}" (envToString evn) fs x (exprToString e)
  | Pair (v1,v2) -> 
      Printf.sprintf "(%s::%s)" (valueToString v1) (valueToString v2) 
  | Nil -> 
      "[]"

and envToString evn =
  let xs = List.map (fun (x,v) -> Printf.sprintf "%s:%s" x (valueToString v)) evn in
  "["^(String.concat ";" xs)^"]"

and exprToString e =
  match e with
      Const i ->
        Printf.sprintf "%d" i
    | True -> 
        "true" 
    | False -> 
        "false"
    | Var x -> 
        x
    | Bin (e1,op,e2) -> 
        Printf.sprintf "%s %s %s" 
        (exprToString e1) (binopToString op) (exprToString e2)
    | If (e1,e2,e3) -> 
        Printf.sprintf "if %s then %s else %s" 
        (exprToString e1) (exprToString e2) (exprToString e3)
    | Let (x,e1,e2) -> 
        Printf.sprintf "let %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 
    | App (e1,e2) -> 
        Printf.sprintf "(%s %s)" (exprToString e1) (exprToString e2)
    | Fun (x,e) -> 
        Printf.sprintf "fun %s -> %s" x (exprToString e) 
    | Letrec (x,e1,e2) -> 
        Printf.sprintf "let rec %s = %s in \n %s" 
        x (exprToString e1) (exprToString e2) 

(*********************** Some helpers you might need ***********************)

let rec fold f base args = 
  match args with [] -> base
    | h::t -> fold f (f(base,h)) t

let listAssoc (k,l) = 
  fold (fun (r,(t,v)) -> if r = None && k=t then Some v else r) None l

(*********************** Your code starts here ****************************)

let lookup (x,evn) = match listAssoc (x,evn) with
  | Some x -> x
  | _ -> raise (MLFailure ("variable not bound: " ^ x))


let rec eval (evn,e) = match e with
    | NilExpr -> Nil
    | Const x -> Int x
    | True -> Bool true
    | False -> Bool false
    | Var x -> lookup(x,evn)
    | Bin(a,o,b) ->
        let a = eval (evn, a) in
        let b = eval (evn, b) in
        begin match (a, o, b) with
          | Int a, Plus, Int b -> Int (a + b)
          | Int a, Minus, Int b -> Int (a - b)
          | Int a, Mul, Int b -> Int (a * b)
          | Int a, Div, Int b -> Int (a / b)
          | Int a, Eq, Int b -> Bool (a = b)
          | Int a, Ne, Int b -> Bool (a <> b)
          | Int a, Lt, Int b -> Bool (a < b)
          | Int a, Le, Int b -> Bool (a <= b)
          | Bool a, Eq, Bool b -> Bool (a = b)
          | Bool a, Ne, Bool b -> Bool (a <> b)
          | Bool a, And, Bool b -> Bool (a && b)
          | Bool a, Or, Bool b -> Bool (a || b)
          | Int a,Cons, Nil -> Pair (Int a, Nil) (*Extra Cred*)
          | Int a, Cons, Pair(x,y) -> Pair (Int a, b) (*Extra Cred*)
          | _ -> raise (MLFailure ("Invalid Operation"))
        end
    | If(p, t, f) -> let Bool cond = eval(evn,p) in
        if cond then eval(evn,t) else eval(evn,f)
    | Let(b,e1,e2) -> let evn = (b, eval(evn, e1))::evn in eval(evn, e2)
    | Letrec(b,e1,e2) ->
      let v = eval (evn, e1) in
      let evn1 = (
        match v with
        | Closure (evn',None,x,e) -> Closure (evn',Some b,x,e)
        | _ -> v
      ) in let evn2 = (b,evn1)::evn in eval (evn2,e2)
    | App (a,b) -> (match a with
      | Var "hd" -> (match eval (evn,b) with Pair (x,y) -> x)
      | Var "tl" -> (match eval (evn,b) with Pair (x,y) -> y)
      | _ -> match eval (evn,a) with
       | Closure (place,choice,domain,range) -> (match choice with
        | Some i -> eval (((i, Closure(place,choice,domain,range))::((domain,eval (evn,b))::place)),range)
        | None -> eval (((domain, eval (evn,b))::place),range)))
    | Fun(x,e) -> Closure(evn, None, x, e)
(**********************     Testing Code  ******************************)
