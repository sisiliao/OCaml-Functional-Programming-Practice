(* Student information:

   Name: SIYUN LIAO
   McGill ID: 260584769

 *)

(* Notice: by submitting as part of team, you declare that you worked
   together on the solution. Submissions in pairs are allowed to
   foster team work, they have to be developed by both students *)

(* Homework 1 - Questions 2 and 3 *)

(* First, some utility functions and declarations that you can use. Be
   sure to check Ocaml's documentation to find more functions
   available to you.

   You can start checking the documentation at:
   https://caml.inria.fr/pub/docs/manual-ocaml/libref/Pervasives.html
 *)

(* the value of pi *)
let pi : float =  acos ~-.1.0

(* a function to compare floats that allows for some imprecision *)
let cmp n m = abs_float (n -. m) < 0.0001

(* a simple test of positivity *)
let positive a = a > 0.0

(* a function to check if a is multiple of b *)
let is_multiple_of (a : float) (b : float) : bool =
  let m = a /. b in
  cmp (m -. floor m) 0.0

(* a function to check if a is between plus/minus b *)
let abs_btwn a b = a < b && a > ~-.b

(* Question 2: Triangles are the best *)

type side = float

type tr_by_sides = side * side * side

type tr_kind
  = Scalene
  | Equilateral
  | Isosceles

(* Question 2.1 *)
let well_formed_by_sides (a, b, c : tr_by_sides) : bool =
  	(positive a && positive b && positive c) &&	
		(a +. b>c && a +. c>b && b +. c>a)                 
(* write your solutions in place of all the "assert false" *)


(* Question 2.2 *)
let create_triangle (kind : tr_kind) (area : float) : tr_by_sides =
  match kind with
|Scalene -> (sqrt(2.0 *. area /. (sin(pi /. 3.0) *. sin(pi /. 6.0))),(sqrt(2.0 *. area /. (sin(pi /. 3.0) *. sin(pi /. 6.0))) *. sin(pi /. 3.0)), (sqrt(2.0 *. area /. (sin(pi /. 3.0) *. sin(pi /. 6.0))) *. sin(pi /. 6.0)) )
|Equilateral -> (sqrt(2.0 *. area /. sin(pi /. 3.0)),sqrt(2.0 *. area /. sin(pi /. 3.0 )) , sqrt(2.0 *. area /. sin(pi /. 3.0)) )
|Isosceles -> ((sqrt(area *. 2.0))  ,(sqrt(area *. 2.0)) ,sqrt(2.0) *. (sqrt(area *. 2.0)) )

(* Question 2.3 *)
type angle = float

type tr_by_angle = side * side * angle

let well_formed_by_angle (a, b, gamma) : bool =
	(positive a && positive b && positive gamma ) &&
		(not (is_multiple_of gamma pi))

let angle_by_cos_law a b c : angle = 
	acos ((a *. a +. b *. b -. c *. c) /. (2.0 *. a *. b))

let side_by_cos_law a b gamma : side =
	sqrt(a *. a +. b *. b -. 2.0 *. a *. b *. cos(gamma))

let sides_to_angle (a,b,c : tr_by_sides) : tr_by_angle option =
	match well_formed_by_sides (a,b,c) with 
	| false -> None
	| true -> Some (a,b,(angle_by_cos_law a b c))

let angle_to_sides (a,b,gamma: tr_by_angle) : tr_by_sides option =
	match well_formed_by_angle(a,b,gamma) with
	| false -> None
	| true -> Some (a,b,(side_by_cos_law a b gamma))


(* Now that you implemented Q2.2 and saw the new representation of
   triangles by two sides and the angle between them, also ponder on
   how one represents Q2.2 using this representation. The idea is to
   think about how the representation helps make illegal states hard
   to represent and how easy and hard it is to implement the
   algorithm. *)

(* Question 3: Flexing recursion and lists *)

let even (n : int) : bool = n mod 2 = 0

(* Question 3.1 *)

let evens_first (l:int list) : int list = 
	(List.filter (fun x -> x mod 2 == 0) l )
	@(List.filter (fun x -> x mod 2 != 0) l )


let ex_1 = evens_first [7 ; 5 ; 2; 4; 6; 3; 4; 2; 1]
(* val ex_1 : int list = [2; 4; 6; 4; 2; 7; 5; 3; 1] *)

(* Question 3.2 *)

let even_streak (l: int list) : int =
	let rec int_list l acc li = match l with
		|[] -> li
		|x::[] -> if even x then (acc+1)::li
			else acc::li
		|x::xs -> if even x then int_list xs (acc+1) li
			else 
				(let li = acc::li in
				  (int_list xs 0 li))
	in 
	let rec list_max (l:int list) = match l with
		|[] -> 0
		|x::xs -> Pervasives.max (x) (list_max xs)
	in
	list_max (int_list l 0 [])

let ex_2 = even_streak [7; 2; 4; 6; 3; 4; 2; 1]

(* val ex_2 : int = 3 *)


(* Question 3.3 *)

type nucleobase = A | G | C | T

let compress (l:nucleobase list) : (int * nucleobase) list =
	let rec gosh l base acc li = match l with 
		|[] -> li
		|x::[] -> if x = base then (acc+1,base)::li
					else (1,x)::((acc,base)::li)
		|x::xs -> (match acc with
					|(-1) ->  gosh l x 0 []
					|_ -> if x = base then gosh xs base (acc+1) li
							else 
							(let newli = (acc, base)::li in
							(gosh xs x 1 newli))
				  )			
	in List.rev(gosh l A (-1) [])

let rec decompress (l: (int * nucleobase) list) : nucleobase list =
match l with 
	|[] -> []
	|(x,y)::tl -> match x with 
		|0 -> decompress tl 
		|_ -> y::(decompress (((x-1),y)::tl))

let sample_dna : nucleobase list = [A;A;A;A;G;G;A;T;T;T;C;T;C]

let ex_3 = compress sample_dna

let ex_4 = decompress ex_3

let res_3_4 = sample_dna = ex_4 (* This should be true if everything went well *)
