# An OCaml Matrix Library

Purely symbolic matrix manipulation suite comprised of **45 operators** for matrix (OCaml `array`) creation, manipulation and calculation—mostly translations of Array library functions with some interesting additions. Will likely continue to grow as it's used.



## Usage

In general, the leftmost char of an operator signals the associativity/left argument for the prefix or infix operators. The middle character denotes the operation being carried out, and the rightmost character signals the righthand argument for infix operators.
```
   ~      prefix - if first character of operator is ~ then there is only 1 argument, postfixed (~? abc) 
   !      prefix - same as ~ but for matrix operations (!? mat)
   ^      vector - the argument on that side is a vector (vec ^?? ... or ... ??^ vec)
   @      matrix - same as ^ but for matrices (... ??@ mat or mat @?? ...)
   |      scalar - same as ^,@, used when a scalar is required as an argument (ex. mat @^| 3 or 2 |*| 1.0)

  ~?>     print operator for matrix/vector (ex. ~@> mat, ~^> vec)
```



## Operators

It appears incomprehensible at first but you may find that there's some method to the madness.
``` ocaml
module Matlib : sig

  type vector = float array   
  type matrix = vector array                            

  val ( ~|+  ) : 'a list -> 'a array              (* transform list to array *)
  val ( ~|-  ) : 'a array -> 'a list              (* transform array to list *)

  val ( ~.   ) : vector -> vector                 (* get copy a vector *)
  val ( ~..  ) : matrix -> matrix                 (* get copy a matrix *)

  val ( >~<  ) : 'a array -> 'b array -> ('a * 'b) array  (* zip arrays to tupled array *)

  val ( ~||  ) : 'a array -> int                  (* get length of vector, or #rows of matrix *)
  val ( ~||| ) : matrix -> int * int              (* get (r,c) size of matrix *)
  
  val ( |.   ) : vector -> int * float -> unit    (* mutable: modify index in vector *)
  val ( |..  ) : matrix -> int * int * float -> unit      (* mutable: modify index in matrix *)
  val ( |... ) : matrix -> (int*int*float) list -> unit   (* mutable: modify many indices *)
  
  val ( |*|  ) : int -> float -> vector           (* create vector *)
  val ( |**| ) : int * int -> float -> matrix     (* create matrix *)
  val ( ~|**|) : int -> matrix                    (* create identity matrix of size *)
  
  val ( |-|  ) : vector -> int -> vector          (* remove n head elements *)
  val ( |-^  ) : int -> 'a array -> 'a array      (* remove index from v, or remove row from m *)
  val ( |-@  ) : int * int -> matrix -> matrix    (* remove row,col from matrix *)

  val ( @><| ) : matrix -> int * int -> matrix    (* swap rows in matrix *)
  val ( @>.<|) : matrix -> int * int -> unit      (* mutable: swap rows in matrix *)

  val ( ^::^ ) : vector -> vector -> vector       (* horizontally join vectors *)
  val ( ^::@ ) : vector -> matrix -> matrix       (* add row to top of matrix *)
  val ( >::@ ) : vector -> matrix -> matrix       (* add column to front of matrix *)
  val ( @::^ ) : matrix -> vector -> matrix       (* add row to bottom of matrix *)
  val ( @::< ) : matrix -> vector -> matrix       (* add column to right of matrix *)
  val ( @::@ ) : matrix -> matrix -> matrix       (* horizontally join matrices *)
  
  val ( |*^  ) : float -> vector -> vector        (* scale vector *)
  val ( |*@  ) : float -> matrix -> matrix        (* scale matrix *)  
  val ( ^*^  ) : vector -> vector -> float        (* v * v *)
  val ( ^+^  ) : vector -> vector -> vector       (* v + v *)
  val ( ^-^  ) : vector -> vector -> vector       (* v - v *)
  val ( ^=^  ) : vector -> vector -> bool         (* vector equality *)
  val ( @*^  ) : matrix -> vector -> vector       (* m * v *)
  val ( @*@  ) : matrix -> matrix -> matrix       (* m * m *)
  val ( @+@  ) : matrix -> matrix -> matrix       (* m - m *)
  val ( @-@  ) : matrix -> matrix -> matrix       (* m - m *)
  val ( @/@  ) : matrix -> matrix -> matrix       (* (m^-1) * m *)
  val ( @=@  ) : matrix -> matrix -> bool         (* matrix equality *)
  val ( @^|  ) : matrix -> int -> matrix          (* matrix exponent *)

  val ( !|   ) : matrix -> float                  (* determinant *)
  val ( !^   ) : matrix -> float                  (* trace *) 
  val ( !~   ) : matrix -> matrix                 (* transpose *)
  val ( !??  ) : matrix -> bool                   (* intertability test *)
  val ( !?   ) : matrix -> matrix                 (* invert matrix *)
  val ( ~@@  ) : matrix -> matrix                 (* mutable: gaussian elimination *)

  val ( ~^>  ) : vector -> unit                   (* print vector *)
  val ( ~@>  ) : matrix -> unit                   (* print matrix *)

end
```

----

#### Implementation

The current form of ACamlMatLib uses native floats but can be easily changed by altering the block defining literals:
``` ocaml
  type vector = float array 

  let zero = 0. and one = 1. 
  and absol = fun x -> abs_float x
  and elprint = fun e -> sprintf "%0.3f\t" e
  and epsilon = 1e-5
  and ( &+ ) a b = a+.b
  and ( &- ) a b = a-.b
  and ( &* ) a b = a*.b
  and ( &/ ) a b = a/.b
```
