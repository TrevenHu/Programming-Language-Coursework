(* CSE 130: Programming Assignment 1
 * misc.ml
 *)

(* sumList : int list -> int 
   * sumList(l) is the sum of each element of the given list l
   e.g. (sumlist [1;2;3;4]) is 10
*) 

let rec sumList l = 
	match l with
	| [] -> 0
	| h :: t -> h+(sumList t);;

(* digitsOfInt : int -> int list 
   * (digintsOfInt n) is the list of digits of n (n is non-negative number)
   * in the order in which they appear in n
 *)

let rec digitsOfInt n = 
	let rec dig tail n =
	if n<10 then n::tail
	else dig((n mod 10)::tail)(n/10) in 
	dig [] n;;

(* digits : int -> int list
 * (digits n) is the list of digits of n in the order in which they appear
 * in n
 * e.g. (digits 31243) is [3,1,2,4,3]
 *      (digits (-23422) is [2,3,4,2,2]
 *)
 
let digits n = digitsOfInt (abs n)


(* From http://mathworld.wolfram.com/AdditivePersistence.html
 * Consider the process of taking a number, adding its digits, 
 * then adding the digits of the number derived from it, etc., 
 * until the remaining number has only one digit. 
 * The number of additions required to obtain a single digit from a number n 
 * is called the additive persistence of n, and the digit obtained is called 
 * the digital root of n.
 * For example, the sequence obtained from the starting number 9876 is (9876, 30, 3), so 
 * 9876 has an additive persistence of 2 and a digital root of 3.
 *)


(* additivePersistence : int -> int 
 * (additivePersistence n) is the additive persistence 
 * with the given positive integer n
 * e.g. (additivePersistence 9876) is 2

 * digitalRoot : int -> int
 * (digitalRoot n) is the digital root of n
 * e.g. (digitalRoot 9876) is 3
*)

let rec additivePersistence n = 
	(if n>=10 then additivePersistence(sumList(digits(n)))
	 else ~-1) +1;;

let rec digitalRoot n = 
	if n>=10 then digitalRoot(sumList(digits(n)))
	else n;; 

  (* listReverse : 'a list -> 'a list
   * (listReverse l) is the list of the elements of l in the reversed order
   * e.g. (listReverse [1;2;3;4]) is [4:3:2:1]
  *)

let rec listReverse l = 
	let rec rev tail l=
	match l with 
	| [] -> tail
	| h::t -> rev (h::tail) t in
	rev [] l;;

(* explode : string -> char list 
 * (explode s) is the list of characters in the string s in the order in 
 *   which they appear
 * e.g.  (explode "Hello") is ['H';'e';'l';'l';'o']
 *)
let explode s = 
  let rec _exp i = 
    if i >= String.length s then [] else (s.[i])::(_exp (i+1)) in
  _exp 0

(* palindrome : string -> bool 
 * A palindrome is a word that reads the same from left-to-right and right-to-left.
 * (palindrome w) is true if the string w is palindrome and false otherwise
 * e.g. (palindrome "malayalam") is true
 * e.g. (palindrome "") is true
 * e.g. (palindrome "Malayalam") is false
*)
let palindrome w = 
	if explode w = listReverse (explode(w)) then true
    else false;; 

(************** Add Testing Code Here ***************)
