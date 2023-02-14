https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
exception NotImplemented
let domain () =
    failwith "REMINDER: You should not be writing tests for undefined values."


(*----------------------------------------------------------------*)
(* Q1: String to Characters to String- Implementation of tabulate *)
(* tabulate f n returns [f 0; f 1; ...; f (n - 1)]                *)
(*----------------------------------------------------------------*)

let rec tabulate f n =
  let rec tab n acc =
    if n < 0 then acc
    else tab (n - 1) ((f n) :: acc)
  in
  tab (n - 1) []

(*---------------------------------------------------*)
(* Question 2 : Unfolding is like folding in reverse *)
(*---------------------------------------------------*)

let rec unfold (f : 'seed -> 'a * 'seed) (stop : 'seed -> bool) (b : 'seed) : 'a list =
  if stop b then
    []
  else
    let (x, b') = f b in
    x :: unfold f stop b'

(** Example function using `unfold`.
 * Generates a list of natural numbers less than `max`.
 * *)
let nats max = unfold (fun b -> (b, b+1)) (fun b -> max <= b) 0

(*----------------------------------------*)  
(* Question 3 : Let's *safely* have cake! *)
(*----------------------------------------*)

type price = float
type weight = float
type calories = int
type ingredient = Nuts | Gluten | Soy | Dairy

type cupcake = Cupcake of price * weight * calories * ingredient list

let c1 = Cupcake (2.5, 80.3, 250, [Dairy; Nuts])
let c2 = Cupcake (2.75, 90.5, 275, [Dairy; Soy])
let c3 = Cupcake (3.05, 100.4, 303, [Dairy; Gluten; Nuts])
let c4 = Cupcake (3.25, 120.4, 330, [Dairy; Gluten ])

let cupcakes = [c1 ; c2 ; c3 ; c4]