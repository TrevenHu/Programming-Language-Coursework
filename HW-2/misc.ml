(* CSE 130: Programming Assignment 2
 * misc.ml
 *)

(* assoc : 'a * 'b * ('b * 'a) list -> 'a = <fun> 
 * 
*)

let rec assoc (d,k,l) = 
	match l with
	| [] -> d
	| (h,v)::t -> if h=k then v else assoc(d,k,t)

(* fill in the code wherever it says : failwith "to be written" *)
let removeDuplicates l = 
  let rec helper (seen,rest) = 
      match rest with 
      | [] -> seen
      | h::t -> 
        let seen' = if List.mem h seen then []@seen else [h]@seen in
        let rest' = t in
	  helper (seen',rest') 
  in
      List.rev (helper ([],l))


(* Small hint: see how ffor is implemented below *)
let rec wwhile (f,b) = 
	let result = f b in
		match result with
		|(x_Int, x_Bool) -> if x_Bool then wwhile (f, x_Int) else x_Int

(* fill in the code wherever it says : failwith "to be written" *)
let fixpoint (f,b) = wwhile ((
	let xx b = 
		let tmp = f b in
			if b == tmp then (tmp, false) else (tmp, true) in xx),b)


(* ffor: int * int * (int -> unit) -> unit
   Applies the function f to all the integers between low and high
   inclusive; the results get thrown away.
 *)

let rec ffor (low,high,f) = 
  if low>high 
  then () 
  else let _ = f low in ffor (low+1,high,f)
      
(************** Add Testing Code Here ***************)
