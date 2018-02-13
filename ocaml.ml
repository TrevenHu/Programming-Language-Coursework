let rec insert l i =
	match l with
	| [] -> [i]
	| (h::t) -> if h < i then h::(insert t i)
				else i::(h::t)


let insertion_sort = fun l ->
	let base = [] in
	let fold_fn acc elmt = insert acc elmt in 
	List.fold_left fold_fn base l;;


type expr =
| Var of string
| Const of int
| Plus of expr * expr


let rec simpl e =
	match e with
    | Plus (e1,e2) -> 
	    let e1' = simpl e1 in
	    let e2' = simpl e2 in
		    match (e1',e2') with
		    | Const c1, Const c2 -> Const (c1+c2)
		    |_ -> Plus(e1',e2')
    |_ -> e ;;

 simpl (Plus (Plus (Const 20, Const 10), Var "a"));;

let count f l = 
	let base = 0 in
	let fold_fn acc elmt = if f elmt then acc+1 else acc in
	List.fold_left fold_fn base l;;

let stretch l =
	let base = [] in
	let fold_fn acc elmt = acc@[elmt;elmt] in
	List.fold_left fold_fn base l;;


type 'a tree =
| Empty
| Node of 'a * 'a tree list;;

let rec zip l1 l2 =
	match (l1,l2) with
	| ([],[]) -> []
	| (h1::t1, h2::t2) -> (h1,h2)::(zip t1 t2)
	| _ -> [];;

let rec tree_zip (t1,t2) =
	match (t1,t2) with
	| (Node(t1',[]),Node(t2',[])) -> Node((t1',t2'),[])
	| (Node(t1',l1),Node(t2',l2)) -> 
			Node((t1',t2'),List.map tree_zip (zip l1 l2))
	| (_,_) -> Empty;;

let rec sum_matrix m =
	let fold_fn acc elmt = acc@elmt in
    let l = List.fold_left fold_fn [] m in
    let base = 0 in
    let fold a e = a+e in
    List.fold_left fold base l;;


let rec find d k =
	match d with
	| [] -> raise Not_found
	| (k',v') :: t -> if k' = k then v' else find t k;;


let rec add d k v =
	match d with
	| [] -> [(k,v)]
	| (k',v') :: t -> if k = k' then (k,v)::t 
					  else if k < k' then (k,v)::((k',v')::t)
					  else (k',v') :: (add t k v) ;;

let keys d =
	let pick (x,y) = x in
	List.map pick d;;


let values d = 
	let pick (x,y) = y in
	List.map pick d;;

let key_of_max_val d =
	let fold_fn (k,v) (k',v') = if v' > v then (k',v')
								else (k,v) in
	match d with
	| [] -> raise Not_found
	| base :: t -> let (max,_)=List.fold_left fold_fn base d in max;;





