(* Question 1 - Unfolding *)

(* This is the function unfold, take some time to compare it it fold.
   If fold_left and fold_right take lists to generate a value, this
   function takes a value that will generate a list. The relation
   between folds and unfolds is the beginning of a wonderful tale in
   computer science. But let's not get ahead of ourselves.

   Unfold takes a function that from a seed value it generates a new
   element of the list, and a the seed for the next element, another
   function to stop the generation, and an initial seed.
*)

let rec unfold (f: 'seed -> ('a * 'seed)) (stop : 'b -> bool) (b : 'seed) : 'a list =
  if stop b then []
  else let x, b' = f b in
       x :: (unfold f stop b')

let nats max = unfold (fun b -> b, b + 1) (fun x -> x > max) 0

(* Q1.1: Return the even numbers up-to max *)
let evens max = unfold (fun b -> b, b + 2) (fun x -> x > max) 0

(* Q1.2: Return the Fibonacci sequence up-to max *)

let fib max = unfold (fun (c,d)->c,(d,c+d))(fun (c,d)-> max<c ) (1,1)

(* Q1.3: Return the list of rows of the Pascal triangle that are shorter than max *)
let rec add2Lists l1 l2 =
  match (l1, l2) with
  |([],_) -> l2
  |(_,[]) -> l1
  |(x::txs, y::tys) -> (x+y)::(add2Lists txs tys)

let rec pascal_row n =
  let rec aux n acc =
    if n = 0 then acc
    else 
      aux (n-1) (add2Lists (0::acc) acc)
  in 
  aux n [1]

let pascal max = unfold (fun b->pascal_row b, b+1) (fun b-> max < b) 0


let rec zip (l1 : 'a list) (l2 : 'b list) :  ('a * 'b) list =
match l1, l2 with
| [], _ -> []
| _, [] -> []
| x::xs, y::ys -> (x, y):: zip xs ys

(* (Extra credit) Optional: implement zip with a single call to unfold *)
let zip' l1 l2 = unfold (fun (x::a1,y::a2)-> (x,y),(a1,a2)) (fun (b1,b2) -> (List.length b1 =0)||(List.length b2 = 0)) (l1,l2) 


(* Question 2 *)

let ugly x =
  let rec ackermann m n = match (m , n) with
    | 0 , n -> n+1
    | m , 0 -> ackermann (m-1) 1
    | m , n -> ackermann (m-1) (ackermann m (n-1))
  in
  ackermann 3 x

let memo_zero (f : 'a -> 'b) : 'a -> 'b = f

(* Q2.1: Write a function that memoizes the last value called. *)
let memo_one (f : 'a -> 'b) : ('a -> 'b) =
  let result = ref (0,0) in
  fun a' ->
    let (t,p) = !result in
    if (a'=t) then p
    else (let i= f a' in
     result := (a',i); i)

(* Example usage:

let ugly' = memo_one ugly

let u1 = ugly' 3                (* this one calls ugly with 3 *)
let u2 = ugly' 3                (* this one uses the stored value *)
let u3 = ugly' 1                (* the stored value is for 3 so it calls ugly *)
let u4 = ugly' 2                (* the stored value is for 1 so it calls ugly *)
let u5 = ugly' 10               (* the stored value is for 2 so it calls ugly and takes a couple of seconds *)
let u6 = ugly' 10               (* the one uses the stored value and returns immediately *)

 *)

(* Q2.2: Write a function that memoizes the last value called. *)

let memo_many (n : int) (f : 'a -> 'b) : 'a -> 'b =
  let result_array = Array.make n None in
  let ptr = ref 0 in
  let update a newdata = if ((Array.length result_array)-1) < !ptr then
                          (Array.set result_array 0 (Some (a,newdata)); ptr := 1; newdata)
                        else
                          (Array.set result_array !ptr (Some (a,newdata)); ptr := !ptr+1; newdata)
  in
  let rec search count f a = if ( count< (Array.length result_array)) then
                          (match Array.get result_array count with
                                |None -> let newdata = (f a) in
                                         (Array.set result_array !ptr (Some (a,newdata));
                                         ptr := (!ptr+1); newdata)
                                |Some (w,s) -> if w=a then s
                                                else search (count+1) f a
                          )
                         else(count=0; let newdata = (f a) in
                          update a newdata)
  in search 0 f



(* Question 3: Doubly-linked circular lists  *)

(* Circular doubly linked lists *)

(* The type of a cell (a non-empty circular list) *)
type 'a cell = { mutable p : 'a cell; data : 'a ; mutable n : 'a cell}

(* The type of possibly empty circular lists *)
type 'a circlist = 'a cell option

(* An empty circular list *)
let empty :'a circlist = None

(* A singleton list that contains a single element *)
let singl (x : 'a) : 'a circlist =
  let rec pointer = {p = pointer ; data = x ; n = pointer} in
  Some pointer

(* Rotate a list to next element *)
let next : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.n)

(* Rotate a list to previous element *)
let prev : 'a circlist -> 'a circlist = function
  | None -> None
  | Some cl -> Some (cl.p)

(* Q3.1: Write a function that add a new element at the beginning of a list *)
let cons (x : 'a)  (xs : 'a circlist) : 'a circlist = match xs with
  | None -> singl x
  | Some cl ->
    let single = singl x in
    match single with
    | None -> None
    | Some newnode ->
    newnode.n <- cl; newnode.p <- cl.p; cl.p.n <- newnode; cl.p <- newnode; xs

(* Q3.2: Write a function that computes the length of a list (Careful with the infinite loops)  *)
let rec length (l : 'a circlist) : int =
  match l with
    |None -> 0
    |Some cl -> let rec helper l0 count = match l0 with
                  |None -> 1
                  |Some cc -> if cc==cl then (count+1)
                              else helper (next l0) (count+1)
                in helper (next l) 0


(* Q3.3: Write a function that produces an immutable list from a circular list *)

let to_list (l : 'a circlist)  : 'a list =
  match l with
    |None -> []
    |Some cl -> let rec helper l0 acc = match l0 with
                  |None -> []
                  |Some cc -> if cc==cl then cl.data::acc
                              else helper (next l0) (acc@[cc.data])
                in helper (next l) []



(* Once you've written cons you can use this function to quickly populate your lists *)
let rec from_list : 'a list -> 'a circlist = function
  | [] -> empty
  | x::xs -> cons x (from_list xs)

(* Q3.4: Write a function that reverses all the directions of the list *)
let rev (l : 'a circlist) : 'a circlist =
  match l with
   |None -> None
   |Some cl -> let rec helper l0 = match l0 with
                |None -> None
                |Some cc -> if cc==cl then (let temp = cc.n in
                            cc.n <- cc.p; cc.p <- temp; (Some cl))
                            else (let temp = cc.n in
                            cc.n <- cc.p; cc.p <- temp; (helper (prev l0)))
                in helper (next l)

(* (Extra credit) OPTIONAL: Write the map function as applied to lists *)
let map (f : 'a -> 'b) : 'a circlist -> ' b circlist = assert false

(* Some possibly useful functions (Wink, wink!) *)

(* A function that returns the Greatest Common Denominator of two numbers *)
let rec gcd (u : int) (v : int) : int =
  if v <> 0 then (gcd v (u mod v))
  else (abs u)

(* A function that returns the least common multiple of two numbers *)
let lcm (m : int) (n : int) : int  =
  match m, n with
  | 0, _ | _, 0 -> 0
  | m, n -> abs (m * n) / (gcd m n)


(* (Extra credit) OPTIONAL A function that compares two lists ignoring the rotation *)
let eq (l1 : 'a circlist) (l2 : 'a circlist) : bool = assert false

(* Some examples *)
(*
let ex = cons 12 (cons 43 (cons 34 (singl 3)))
let lex = to_list ex

let l1 = from_list [true; true ; false]
let l3 = from_list [true; true ; false ; true; true ; false]

let l4 = from_list ['a'; 'b'; 'a'; 'b']
let l5 = from_list ['a'; 'b'; 'a'; 'b'; 'a'; 'b']

let l6 = from_list ['a'; 'a']
let l7 = from_list ['a'; 'a'; 'a']

let l8 = from_list [1 ; 2 ; 3]
let l9 = from_list [3 ; 1 ; 2]  (* eq l8 l9 = true *)

let l10 = from_list [1 ; 2 ; 3]
let l11 = from_list [3 ; 2 ; 1]  (* eq l10 l11 = false *)
*)
