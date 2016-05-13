
#use "matlib.ml";;
module ML = Make(struct 
  type elt = float
  let zero = 0. and one = 1. 
  and absol = fun x -> abs_float x
  and eltformat = fun e -> sprintf "%0.3f\t" e
  and t_eq a b = abs_float (a -. b) < 1e-7
  and t_add a b = a+.b
  and t_sub a b = a-.b
  and t_mul a b = a*.b
  and t_div a b = a/.b
end);;
open ML;;

(* maybe I'll include this sometime later *)
(* #use "testers/fileio.ml";; *)


type point = float * float;;
type pointset = point list;;



(* return (t, f(t)) pairs from t0 to tf *)
let fx_over_time f (t_lo,t_hi) stepsize : pointset = 
	let rec til_hi t_cur = 
		if t_cur < t_hi then (t_cur,(f t_cur)) :: (til_hi (t_cur+.stepsize)) else []
	in til_hi t_lo;;



(* randcurve : (float -> float) -> (float * float) -> float -> int -> (float * float) list
offset the input, output to a function by a preset amount
return "steps" number of (x,f(x)) points from xlo to xhi *)
let randomized_curve f (xlo, xhi) offset steps = 
	let delta = (xhi-.xlo) /. (float_of_int steps) in
	let () = Random.self_init() in
	let rec makepoints cur = if cur<xhi then (
		let changex = (offset/.2.) -. (Random.float offset) in
		let changey = (offset/.2.) -. (Random.float offset) in
		(changex +. cur, changey +. (f cur)) :: (makepoints (cur+.delta))
	) else [] in makepoints xlo;;



(* define the formula for a least squares fit of raw data *)
let least_squares
	(basisfuns : (float -> float) list)
	(xpoints : float list)
	(fxpoints : float list) : vector = 

	(* generate A matrix with basis functions evaluated at the given xpoints *)
	let a = Array.map (fun xp -> 
		Array.map (fun f -> f xp) (~|+ basisfuns)
	) (~|+ xpoints) in 

	(* generate b vector with given f(x) points *)
	let b = (~|+ fxpoints) in

	(* EQN: (A^T A)x = (A^T)b ---> x = (A^T A)^(-1) * (A^T)b *)
	(* and in the end, all it takes is one line of symbolic code to compute *)
	(!? ((!~ a) @*@ a)) @*^ ((!~ a) @*^ b);;





(* define least squares fit polynomial function *)
let example_curve = fun x -> if x <= 1. then 0. else (((100./.x) *.(sin x))) -. x;;
let xrange = (1.05,10.);;
let error = 1.;;
let numpoints = 100000;;

let points = randomized_curve example_curve xrange error numpoints;;



(* take least squares fit using the cubic polynomial *)
let polyx0 = fun x -> 1.;;
let polyx1 = fun x -> x;;
let polyx2 = fun x -> x*.x;;
let polyx3 = fun x -> x**3.;;
let basisfuns = [ polyx0; polyx1; polyx2; polyx3 ];;
let basisfx bases = fun x -> List.fold_left (fun acc f -> acc +. (f x)) 0. bases;;

let xp = List.map (fun (x,y) -> x) points;;
let yp = List.map (fun (x,y) -> y) points;;

let lsq_params = least_squares basisfuns xp yp;;



(* make a function out of the parameters obtained from the lease squares fit *)
let lsq_function = basisfx 
	(List.map (fun (p,f) -> fun x -> p*.(f x)) (List.combine (~|- lsq_params) basisfuns));;

let lsq_func_points = fx_over_time lsq_function xrange 0.05;;
let example_points = fx_over_time example_curve xrange 0.05;;


(* "lsq_points.dat"||<..lsq_func_points;;
"lsq_randpoints.dat"||<..points;;
"lsq_realpoints.dat"||<..example_points;; *)

