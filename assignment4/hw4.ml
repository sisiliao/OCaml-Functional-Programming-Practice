(* Q1: A Rose by any Other Name Would Smell as Sweet *)

type 'a rose_tree = Node of 'a * ('a rose_tree) list

(* Find with exceptions *)

exception BackTrack

(* Q1.1 write a function that finds an element of the tree using backtracking with exceptions *)

let rec find_e (p : 'a -> bool) (t : 'a rose_tree) : 'a = (* Function with exceptions *)
 match t with
  |Node (x,[]) -> if (p x) then x else raise BackTrack 
  |Node (x,h::t) -> if(p x) then x
		 else try find_e p h with BackTrack -> (find_e p (Node (x,t)))


(* Q1.1: write this function and it helper functions *)
let find (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = (* call find_e and handle the exceptions *)
  try let r = (find_e p t) in Some r
  with BackTrack -> None 
   

(* Find with failure continuations *)

let rec find_k (p : 'a -> bool) (t : 'a rose_tree) (k : unit -> 'a option) : 'a option =
  match t with
  |Node (x,[]) -> if(p x) then Some x else k ()
  |Node (x,h::t) -> if(p x) then Some x
		    else find_k p h (fun () -> find_k p (Node (x,t)) k)

(* Q1.2: write this function and it helper functions *)
let find' (p : 'a -> bool)  (t : 'a rose_tree) : 'a option = (*  call find_k with the appropriate inital continuation *)
find_k p t (fun () -> None)

(* Find all with continuations *)
(*
let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b =
match t with
  |Node (x,[]) -> if (p x) then k [x]
		  else k []
  |Node (x,h::t) -> if (p x) then find_all_k p h (fun el -> el@x::(find_all_k p (Node (x,t)) k))
	 	    else find_all_k p h (fun el -> el@(find_all_k p (Node (x,t)) k))
*)


let rec find_all_k  (p : 'a -> bool) (t : 'a rose_tree) (k : 'a list -> 'b) : 'b =
match t with
  |Node (x,[]) -> if (p x) then k [x]
                  else k []
  |Node (x,h::t) -> if (p x) then find_all_k p h (fun el -> el@(find_all_k p (Node (x,t)) k))
                    else find_all_k p h (fun el -> el@(find_all_k p (Node (x,t)) k))


(* Q1.3: write this function and it helper functions *)
let find_all p t = find_all_k p t (fun l->l)

(* An example to use *)
(*
let example = Node (7, [ Node (1, [Node (22,[Node (23,[])])])
                         ; Node (2, [Node (16, [])])
                         ; Node (4, [])
                         ; Node (9, [])
                         ; Node (11, [])
                         ; Node (15, [])
                         ])

*)

let example = Node (7, [ Node (1, [])
                         ; Node (2, [Node (16, [])])
                         ; Node (4, [])
                         ; Node (9, [])
                         ; Node (11, [])
                         ; Node (15, [])
                         ])

let is_big x =  x > 2


(* Q2 : Rational Numbers Two Ways *)

type fraction = int * int

module type Arith =
  sig
    type t
    val epsilon : t             (* A suitable tiny value, like epsilon_float for floats *)

    val plus : t -> t -> t      (* Addition *)
    val minus : t -> t -> t     (* Substraction *)
    val prod : t -> t -> t      (* Multiplication *)
    val div : t -> t -> t       (* Division *)
    val abs : t -> t            (* Absolute value *)
    val lt : t -> t -> bool     (* < *)
    val le : t -> t -> bool     (* <= *)
    val gt : t -> t -> bool     (* > *)
    val ge : t -> t -> bool     (* >= *)
    val eq : t -> t -> bool     (* = *)
    val from_fraction : fraction -> t (* conversion from a fraction type *)
    val to_string : t -> string        (* generate a string *)
  end

module FloatArith : Arith =
struct
  type t = float
  let epsilon = epsilon_float
  let from_fraction (num, den) = float_of_int num /. float_of_int den

  let plus = (+.)
  let minus = (-.)
  let prod = ( *. )
  let div = ( /. )
  let abs = abs_float
  let lt = (<)
  let le = (<=)
  let gt = (>)
  let ge = (>=)
  let eq = (=)
  let to_string x = string_of_float x
end

(* Q2.1: Implement the Arith module using rational numbers (t = fraction) *)

module FractionArith : Arith =
struct
  type t = fraction
  let epsilon = (1,1000000)
  let from_fraction (num,den) = (num,den)
  
  let gcd (num, den)= 
    let rec aux (num,den) acc = 
	if (num mod acc = 0 && den mod acc = 0) then acc
	else aux (num,den) (acc+1)
    in aux (num,den) 2 
  
  let simplify (num,den) = 
    let aux (num,den) cd = 
	(num/cd, den/cd)
    in aux (num,den) (gcd (num,den)) 
 
  let plus (a,b) (c,d) = ((a * d)+(b * c) , (b * d))
  let minus (a,b) (c,d) = ((a * d)-(b * c) ,(b * d))
  let prod (a,b) (c,d) = ((a * c) , (b * d))
  let div (a,b) (c,d) = ((a * d) , (b * c))
  let abs (a,b) = (Pervasives.abs a, Pervasives.abs b)
  let lt (a,b) (c,d) = (a * d) < (b * c)
  let le (a,b) (c,d) = (a * d) <= (b * c)
  let gt (a,b) (c,d) = (a * d) > (b * c)
  let ge (a,b) (c,d) = (a * d) >= (b * c)
  let eq (a,b) (c,d) = (a * d) = (b * c)
  let to_string (a,b) = "("^string_of_int a ^","^string_of_int b^")"
end


module type NewtonSolver =
  sig
    type t

    val square_root : t -> t
  end

(* Q2.2: Implement a function that approximates the square root using  the Newton-Raphson method *)


module Newton (A : Arith) : (NewtonSolver with type t = A.t) = 
struct 
  type t = A.t
  let square_root n = 
    let rec findroot x acc = 
	let next = A.div (A.plus (A.div n x) x) (A.from_fraction (2,1)) in 
	let diff = A.abs (A.minus x next) in
	if (A.le diff acc) then next
	else findroot next acc
    in findroot (A.from_fraction (1,1)) A.epsilon
end


(* Examples *)

 module FloatNewton = Newton (FloatArith) 
 module RationalNewton = Newton (FractionArith) 

 let sqrt2 = FloatNewton.square_root (FloatArith.from_fraction (2, 1)) 
 let sqrt2_r = RationalNewton.square_root (FractionArith.from_fraction (2, 1)) 

(* Q3 : Real Real Numbers, for Real! *)

type 'a stream = { head : 'a  ; tail : unit -> 'a stream}

let rec nth z = function
  | 0 -> z.head
  | n -> nth (z.tail()) (n - 1)

let rec constant x = {head = x ; tail = fun () -> constant x }

(* Some examples *)

let sqrt2 =
  {head = 1 ; tail = fun () -> constant 2} (* 1,2,2,2,2... *)

let golden_ratio = constant 1   (* 1,1,1,1,1,1 *)

let rec take n z =
  if n = 1 then [z.head]
  else z.head::(take (n-1) (z.tail()))

(* Q3.1: implement the function q as explained in the pdf *)
let rec q z n = match n with
  |0 -> 1
  |1 -> (nth z 1)
  |_ -> (nth z n) * (q z (n-1)) + (q z (n-2)) 

(* Q3.2: implement the function r as in the notes *)
let rec r z n = match n with
  |0 -> float_of_int z.head
  |_ -> -1.0 ** float_of_int (n-1) /.((float_of_int (q z n)) *. float_of_int (q z (n-1)))  +. (r z (n-1))
(*
let rec r z n = match n with
  |0 -> float_of_int z.head
  |_ -> let deno = ((float_of_int (q z n)) *. float_of_int (q z (n-1))) in
	if (deno=0.0) then 10.0
	else (-1.0 ** float_of_int (n-1) /. deno)  +. (r z (n-1))
*)

(* Q3.3: implement the error function *)
let error z n = 
  let deno = float_of_int ((q z n) * (q z n + q z (n-1))) in
  if(deno=0.0) then 0.0
  else 1.0 /. deno

(* Q3.4: implement a function that computes a rational approximation of a real number *)
(*let rat_of_real z approx = 
  let rec ror z approx n = 
    if (error z n > approx) then ror z approx (n+1)
    else r z n
  in ror z approx 1
*)
let rat_of_real z approx =
  let rec ror z approx n =
    if (error z n > approx) then ror z approx (n+1)
    else (if(nth z 1 = 0) then r z 0 else r z n)
  in ror z approx 1
 


let real_of_int n = { head = n ; tail = fun () -> constant 0}

(* Q3.5: implement a function that computes the real representation of a rational number   *)
let rec real_of_rat r = 
  let integer = floor r in
  let fraction = r -. (floor r) in
  if fraction <> 0.0 then
    {head = int_of_float integer; tail = fun () -> real_of_rat (1.0 /. fraction)}
  else
    {head = int_of_float integer; tail = fun () -> constant 0}


(* Examples *)

(* Approximations of the  irrational numbers we have *)

let sqrt_2_rat = rat_of_real sqrt2 1.e-5 
let golden_ratio_rat = rat_of_real golden_ratio 1.e-5 

(* To test the representation of rationals we can try this *)
 let to_real_and_back n = rat_of_real (real_of_rat n) 0.0001

(* e1 should be very close to 10 (it is exactly 10 in the model solution) *)
 let e1 = to_real_and_back 10.0 

(* this is the float approximation of pi, not the real number pi *)
 let not_pi = 2. *. acos 0. 

(* This should share the same 4 decimals with not_pi *)
 let not_pi' = to_real_and_back not_pi 
