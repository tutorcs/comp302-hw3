https://tutorcs.com
WeChat: cstutorcs
QQ: 749389476
Email: tutorcs@163.com
(*--------------------------------------------------------------*)
(* Question 1 : String to Characters to String                  *)
(*--------------------------------------------------------------*)

(* 1.1 Turn a string into a list of characters. *)
let string_explode (s : string) : char list =
  raise NotImplemented

(* 1.2 Turn a list of characters into a string. *)
let string_implode (l : char list) : string =
  raise NotImplemented


(*--------------------------------------------------------------*)    
(* Question 2: unfolding is like folding in reverse             *)
(*--------------------------------------------------------------*)
                     
(* 2.1 Compute the even natural numbers up to an exclusive limit. *)
let evens (max : int) : int list =
  raise NotImplemented

(* 2.2 Compute the fibonacci sequence up to an exclusive limit. *)
let fib (max : int) : int list =
  raise NotImplemented

(* 2.3 Compute Pascal's triangle up to a maximum row length. *)
let pascal (max : int) : int list list =
  raise NotImplemented

(* 2.4 Implement zip, which converts two lists into a list of tuples.
       e.g. zip [1; 2] ['a'; 'c'] = [(1, 'a'); (2, 'c')]
       Note that if one list is shorter than the other, then the 
       resulting list should have the length of the smaller list.     *)

let zip (l1 : 'a list) (l2 : 'b list) : ('a * 'b) list =
  raise NotImplemented

                  
(*--------------------------------------------------------------*)
(* Question 3 : Let's *safely* have cake!                       *)
(*--------------------------------------------------------------*)

(* 3. Return the cupcakes from the cupcake list that contain none of the 
      allergens.                                                         *)
  
let allergy_free (allergens : ingredient list) (cupcakes : cupcake list)
  : cupcake list =
  raise NotImplemented
