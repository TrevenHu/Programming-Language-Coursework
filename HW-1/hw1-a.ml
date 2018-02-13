

let rec sumList l = 
	match l with
	| [] -> 0
	| h :: t -> h+(sumList t);;

let rec digitsOfInt n=
	let rec dig tail n =
		if n<10 then n::tail
		else dig((n mod 10)::tail)(n/10) in 
		dig [] n;;

let digits n = digitsOfInt (abs n);;

let rec additivePersistence n = 
	(if n>=10 then additivePersistence(sumList(digits(n)))
	 else ~-1) +1;;

let rec digitalRoot n = 
	if n>=10 then digitalRoot(sumList(digits(n)))
	else n;; 

let rec listReverse l = 
	let rec rev tail l=
	match l with 
	| [] -> tail
	| h::t -> rev (h::tail) t in
	rev [] l;;

let palindrome w =
	if explode w = listReverse (explode(w)) then true
    else false;; 

