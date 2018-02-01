
type prop = Atom of string
          | Not of prop
          | And of prop * prop
          | Or of prop * prop

let impl (p, q) = Or(Not p, q)
let iff (p, q) = And (impl (p,q), impl (q, p))

let mp = impl (And (Atom "P", impl (Atom "P", Atom "Q")), Atom "Q")

(* Proof by contradiction (reduction ad absurdum): ((¬ P) ⇒ Q) ∧ ((¬ P) ⇒ (¬ Q)) ⇒ P *)
let ra = impl (
             And (impl (Not (Atom "P"),
                        Atom "Q"),
                  impl (Not (Atom "P"),
                        Not (Atom "Q"))),
             Atom "P")

(* Atoms and their negations *)
type signed_atom
  = PosAtom of string
  | NegAtom of string

(* In NNF negations can only be applied to atoms, this datatype only
   allows for propositions in NNF *)
type nnf
  = AndN of nnf * nnf
  | OrN of nnf * nnf
  | AtomN of signed_atom

(* Q1.2: Write the function nnf that converts propositions into NNF,
   notice that the typechecker will guide you in your answer as the
   type forces the result to be in NNF. Your job is to not forget any
   sub-parts of the proposition in the conversion. *)
let rec to_nnf : prop -> nnf = function
  | Atom a 		-> AtomN (PosAtom a)
  | Not (Atom a) 	-> AtomN (NegAtom a)
  | Not (Not p) 	-> to_nnf p
  | Not (And (p,q))	-> to_nnf (Or (Not p, Not q))
  | Not (Or (p,q))	-> to_nnf (And (Not p, Not q))
  | And (p,q)		-> AndN (to_nnf p, to_nnf q)
  | Or (p,q)		-> OrN (to_nnf p, to_nnf q)


(* Q1.3: Write a datatype cnf that represents only propositions in
   cnf. Hint: You might want to use more than one type to be able to
   represent sub-expressions.*)

type dis
  = AtomC of signed_atom
  | Dis of dis * dis

type cnf 
  = AndC of cnf * cnf 
  | OrC of dis  

(* Q1.4: Write the distribute and nnf_to_cnf functions using the new
   datatype. Hint: you may need more than one helper function. *)
let rec distribute : cnf * cnf -> cnf = function
  | p, AndC (q,r)	->AndC (distribute (p,q) , distribute (p,r))
  | AndC (q,r), p	->AndC (distribute (q,p) , distribute (r,p))
  | OrC p, OrC q	->OrC (Dis (p, q))


let rec nnf_to_cnf : nnf -> cnf = function
  | OrN (p,q)	-> distribute (nnf_to_cnf p, nnf_to_cnf q)
  | AndN (p,q)	-> AndC (nnf_to_cnf p, nnf_to_cnf q)
  | AtomN a	-> OrC (AtomC a)

let to_cnf (p :prop) : cnf = nnf_to_cnf (to_nnf p)

(* Q1.5: Write the new positives and negative atoms function as in the
   previous version *)
let rec positives = function
  | AtomC (PosAtom a)->[a] 
  | AtomC (NegAtom a)->[]
  | Dis (p,q) ->positives p @ positives q 

let rec negatives = function
  | AtomC (PosAtom a)->[]
  | AtomC (NegAtom a)->[a]
  | Dis (p,q) -> negatives p @ negatives q

(* Fill in the code for the intersection function from Q1.1 *)

let rec intersection (l1 : 'a list) (l2 : 'a list) : 'a list = match l1 with
        |[] -> []
        |x::xs -> let rec check a (li: 'a list) : 'a list = match li with
                        |[] -> []
                        |h::t -> if h<>a then check a t
                                else h::[]
                  in (check x l2)@intersection xs l2



(* Q1.6: Write the new cnf_tautology function *)
let rec cnf_tautology : cnf -> bool = function
  | AndC (p,q) -> cnf_tautology p && cnf_tautology q
  | (OrC p) -> not ([] = intersection (positives p) (negatives p))

let taut (p : prop) : bool = cnf_tautology (to_cnf p)
let unsat (p : prop) : bool = taut (Not p)
let sat (p : prop) : bool = not (unsat p)


