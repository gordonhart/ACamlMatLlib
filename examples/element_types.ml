
(* 
module FloatSets = struct 
  type t = float
  let zero = 0. and one = 1. 
  and absol = fun x -> abs_float x
  and elprint = fun e -> sprintf "%0.3f\t" e

  and t_eq a b = absol (e1&-e2)) < 1e-7

  and t_add a b = a+.b
  and t_sub a b = a-.b
  and t_mul a b = a*.b
  and t_div a b = a/.b
end
 *)

(* 
module IntSets = struct
  type elt = int
  let zero = 0 and one = 1 and absol = abs
  and elprint = sprintf "%d\t"
  and t_eq a b = a=b
  and t_add a b = a+b
  and t_sub a b = a-b
  and t_mul a b = a*b
  and t_div a b = a/b
end
 *)


(* black is treated as 1 (value 0), white 0 (value 255), and gray in between (values 1-254) *)
type gray = White | Gray of int | Black

module PixelSets = struct 

  type elt = gray
  (* type elt = White | Gray of int | Black *)

  let zero = White and one = Black 

  let verify = function (* make sure a pixel is inbounds *)
    | Gray g -> if g >= 255 then White else begin if g <= 0 then Black else Gray g end
    | edge -> edge

  let absol = fun x -> verify x (* identity *)
  
  let t_eq a b = match a,b with
    | White,White | Black,Black -> true
    | Gray g1,Gray g2 -> g1=g2
    | _ -> false

  let elprint = function
    | Gray g -> sprintf "%d\t" g 
  	| Black -> "0\t"
  	| White -> "255\t"

  let t_add a b = match a,b with
    | White,White -> White
  	| Black,_ | _,Black -> Black
  	| White,Gray g | Gray g,White -> Gray g
  	| Gray g1,Gray g2 -> verify (Gray (g1+g2))

  let t_sub a b = match a,b with
    | _,Black | White,_ -> White
    | Black,White -> Black
    | Gray g,White -> Gray g
    | Black,Gray g -> verify (Gray (255 - g))
    | Gray g1,Gray g2 -> verify (Gray (g1-g2))

  let t_mul a b = match a,b with (* move towards white when 2 gray *)
    | White,_ | _,White -> White
    | Black,Black -> Black
    | Gray g,Black | Black,Gray g -> Gray g
    | Gray g1,Gray g2 -> verify (Gray (iof (255.*.(((foi g1) /. 255.) *. ((foi g2) /. 255.)))))

  let t_div a b = match a,b with (* move towards black when 2 gray *)
    | White,_ -> White
    | Black,_ | _,White -> Black
    | Gray g,Black -> Gray g
    | Gray g1,Gray g2 -> verify (Gray (iof (255.*.(((foi g1) /. 255.) /. ((foi g2) /. 255.)))))

end

