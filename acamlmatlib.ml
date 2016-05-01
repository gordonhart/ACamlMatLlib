
(* ======================================================================================== *)
(*                                                                                          *)
(*            ------- AN OCAML MATRIX LIBRARY ---------------------                         *)
(*            ------- Calculations and Manipulation of Matrices ---                         *)
(*            ------- Gordon Hart, 2016 ---------------------------                         *)
(*                                                                                          *)
(*    Purely symbolic matrix manipulation suite.                                            *)
(*      EVERYTHING is immutable unless explicitly stated otherwise!                         *)
(*                                                                                          *)
(*    There are 51 operators--should be comprehensible after reading                        *)
(*      through the signature for a few minutes.                                            *)
(*                                                                                          *)
(*    In general, the leftmost char of an operator signals the associativity/left           *)
(*      argument for the prefix or infix operators. The middle character denotes the        *)
(*      operation being carried out, and the rightmost char signals the right assoc         *)
(*      argument for infix operators.                                                       *)
(*                                                                                          *)
(*   ------------------------------------------------------------------------------------   *)
(*            key                                                                           *)
(*   ------------------------------------------------------------------------------------   *)
(*     ~      prefix - if first char of op is ~ then there is only 1 argument, postfixed    *)
(*     !      prefix - same as ~ but for matrix operations                                  *)
(*     ^      vector - whenever this shows up, the argument on that side is a vector        *)
(*     @      matrix - same as ^ but for matrices                                           *)
(*     |      scalar - same as ^,@, used when a scalar is required as an argument           *)
(*     .      mutable operator - whenever . appears, there is mutability at play            *)
(*                                                                                          *)
(*     ~?>    print operator for matrix/vector as described by ?                            *)
(*                                                                                          *)
(* ======================================================================================== *)



module ACamlMatLib : sig

  type vector = float array         
  type matrix = vector array  

  val ( ~|+  ) : 'a list -> 'a array                      (* transform list to array *)
  val ( ~|-  ) : 'a array -> 'a list                      (* transform array to list *)
  val ( ~|++ ) : float list list -> matrix                (* transform list list to matrix *)
  val ( ~|-- ) : matrix -> float list list                (* transform matrix to list list *)

  val ( ~|^  ) : vector -> vector                         (* get copy a vector *)
  val ( ~|@  ) : matrix -> matrix                         (* get copy a matrix *)

  val ( ~^>  ) : vector -> unit                           (* print vector *)
  val ( ~@>  ) : matrix -> unit                           (* print matrix *)

  val ( ~|   ) : 'a array -> int                          (* get length of v, or #rows of m *)
  val ( ~||  ) : matrix -> int * int                      (* get (r,c) size of matrix *)
  val ( ~||? ) : matrix -> bool                           (* rectangular matrix test *)
  val ( ~|=|?) : matrix -> bool                           (* square matrix test *)

  val ( >~<  ) : 'a array -> 'b array -> ('a * 'b) array  (* zip arrays to tupled array *)

  val ( ^..  ) : vector -> int * float -> unit            (* mutable: modify index in vector *)
  val ( ^... ) : vector -> (int * float) list -> unit     (* mutable: modify many vector els *)
  val ( @..  ) : matrix -> int * int * float -> unit      (* mutable: modify index in matrix *)
  val ( @... ) : matrix -> (int*int*float) list -> unit   (* mutable: modify many els in mat *)

  val ( |*|  ) : int -> 'a -> 'a array                    (* create vector *)
  val ( |**| ) : int * int -> float -> matrix             (* create matrix *)
  val ( ~|**|) : int -> matrix                            (* create identity matrix of size *)

  val ( ^-|  ) : 'a array -> int -> 'a array              (* remove el from v, or row from m *)
  val ( ^--| ) : 'a array -> int -> 'a array              (* remove n head elements *)
  val ( @-|  ) : matrix -> int -> matrix                  (* remove col from matrix *)
  val ( @--| ) : matrix -> int * int -> matrix            (* remove row,col from matrix *)
  val ( @><| ) : matrix -> int * int -> matrix            (* swap rows in matrix *)
  val ( @>.<|) : matrix -> int * int -> unit              (* mutable: swap rows in matrix *)

  val ( ^::^ ) : vector -> vector -> vector               (* horizontally join vectors *)
  val ( ^::@ ) : vector -> matrix -> matrix               (* add row to top of matrix *)
  val ( >::@ ) : vector -> matrix -> matrix               (* add column to front of matrix *)
  val ( @::^ ) : matrix -> vector -> matrix               (* add row to bottom of matrix *)
  val ( @::< ) : matrix -> vector -> matrix               (* add column to right of matrix *)
  val ( @::@ ) : matrix -> matrix -> matrix               (* horizontally join matrices *)
  
  val ( |*^  ) : float -> vector -> vector                (* scale vector *)
  val ( |*@  ) : float -> matrix -> matrix                (* scale matrix *)  
  val ( ^*^  ) : vector -> vector -> float                (* v * v *)
  val ( ^+^  ) : vector -> vector -> vector               (* v + v *)
  val ( ^-^  ) : vector -> vector -> vector               (* v - v *)
  val ( ^=^  ) : vector -> vector -> bool                 (* vector equality *)
  val ( @*^  ) : matrix -> vector -> vector               (* m * v *)
  val ( @*@  ) : matrix -> matrix -> matrix               (* m * m *)
  val ( @+@  ) : matrix -> matrix -> matrix               (* m - m *)
  val ( @-@  ) : matrix -> matrix -> matrix               (* m - m *)
  val ( @/@  ) : matrix -> matrix -> matrix               (* (m^-1) * m *)
  val ( @=@  ) : matrix -> matrix -> bool                 (* matrix equality *)
  val ( @^|  ) : matrix -> int -> matrix                  (* matrix exponent *)

  val ( !|   ) : matrix -> float                          (* determinant *)
  val ( !^   ) : matrix -> float                          (* trace *) 
  val ( !~   ) : matrix -> matrix                         (* transpose *)
  val ( !??  ) : matrix -> bool                           (* intertability test *)
  val ( !?   ) : matrix -> matrix                         (* invert matrix *)
  val ( !@@  ) : matrix -> matrix                         (* gaussian elimination *)

end = struct

  type vector = float array (* maybe make this polymorphic later *)
  type matrix = vector array

  let zero = 0. and one = 1. 
  and absol = fun x -> abs_float x
  and elprint = fun e -> sprintf "%0.3f\t" e
  and epsilon = 1e-7
  and (&+) a b = a+.b
  and (&-) a b = a-.b
  and (&* ) a b = a*.b
  and (&/) a b = a/.b

  exception Not_Square of string
  exception Out_of_Bounds of string
  exception Undefined_Error of string
  exception Logical_Error of string
  exception Dimension_Mismatch of string
  exception Singular_Matrix of string

  (* MANIPULATION DEFINITIONS *)
  (* easy accessors to size *)
  let (~|) v : int = Array.length v
  let (~||?) m : bool = (* test for rectangular matrix *)
    Array.fold_left (fun acc r -> acc && ((~|r)=(~|(m.(0))))) true m
  let (~||) m : int * int = if ~||? m then (~|m, ~|(m.(0))) 
    else raise (Logical_Error "in ~|| (matrix size: not a rectangular matrix")
  let (~|=|?) m : bool = let (r,c) = ~||m in r=c (* test if a matrix is square *)

  (* to make arrays less annoying to work with *)
  let (~|+) v : 'a array = Array.of_list v (* create array from list *)
  let (~|-) v : 'a list = Array.to_list v (* create list from array *)
  let (~|++) ll : matrix = let m = ~|+ (List.map (~|+) ll) in
    if (~||? m) then m else raise (Logical_Error "in ~|++ : input not rectangular")
  let (~|--) m : float list list = ~|- (Array.map (~|-) m)

  (* create FRESH copies of vectors/matrices *)
  (* using this with a matrix, only copies the references to rows... use ~|@ for matrices *)
  let (~|^) v : vector = Array.copy v
  let (~|@) m : matrix = Array.map (~|^) (Array.copy m)

  (* MUTABLE setters -- just an alias to a nicer setting notation *)
  (* tupling is so much nicer for many args--no bullshit about parenthesizing *)
  let (^..) v (i,vlu) : unit = v.(i) <- vlu (* shorthlet setting notation *)
  let (^...) v changes : unit = List.iter (fun (i,vlu) -> v^..(i,vlu)) changes
  let (@..) m (r,c,vlu) : unit = m.(r).(c) <- vlu  (* set 2d matrix *)
  let (@...) m changes : unit = List.iter (fun (r,c,vlu) -> m@..(r,c,vlu)) changes

  (* zip two arrays into a single tupled array *)
  let (>~<) v1 v2 : ('a * 'b) array = 
    if ((~|v1)=(~|v2)) then Array.mapi (fun i x -> (x,v2.(i))) v1 
    else raise (Dimension_Mismatch "in >~<")

  (* remove an index from 'a array, return shorter version *)
  let (^-|) v i : 'a array = (* VERY SLOW *)
    let remove_index ind vec = 
      let inc = ref (-1) in let inc_is loc = inc := !inc + 1; !inc = loc in
      Array.fold_left (fun a x -> if inc_is ind then a else Array.append a [|x|]) [||] vec
    in try if (i < (~| v)) then remove_index i v else raise (Out_of_Bounds "in |-^") with
    | Out_of_Bounds e -> printf "i: %d\n||v||: %d\n" i (~|v); raise (Out_of_Bounds e)
    | _ -> raise (Undefined_Error "in |-^ : couldn't remove index from vector")

  (* cut off n elements from the front of vector *)
  let (^--|) v n : 'a array = (* a|-|1 is effectively a call to the tail of the vector *)
    try Array.sub v n ((~| v) - n) 
    with _ -> raise (Undefined_Error "couldn't remove elements from vector")

  (* remove let return matrix without a certain column *)
  let (@-|) m ci : matrix = 
    try Array.map (fun row -> row^-|ci) m
    with _ -> raise (Undefined_Error "couldn't remove col from matrix")

  (* remove let return matrix without a certain row,column *)
  let (@--|) m (ri,ci) : matrix = 
    try (m^-|ri)@-|ci (* Array.map (fun row -> row|-^ci) *)
    with _ -> raise (Undefined_Error "couldn't remove row let col from matrix")

  (* vector/matrix creation symbols *)
  let (|*|) s v : 'a array = (* init array of size s with values v *)
    let makevec size vlu = Array.init size (fun x -> vlu) in
    try makevec s v with _ -> raise (Undefined_Error "couldn't create vector")

  let (|**|) (r,c) v : matrix = 
    let makemat (rows,cols) vlu = 
      Array.map (fun r -> Array.make cols vlu) (Array.make rows None)
    in try makemat (r,c) v with _ -> raise (Undefined_Error "couldn't create matrix")

  let (~|**|) n : matrix = (* identity matrix of size [ n x n ] *)
    let identitymat size =
      Array.mapi (fun i row -> row^..(i,one); row) ((size,size)|**|zero) in
    try identitymat n with _ -> raise (Undefined_Error "couldn't create identity matrix")

  (* immutable vector/matrix modification operators *)
  let (^::^) v1 v2 : vector = 
    try Array.append v1 v2 (* array append infix operator *)
    with _ -> raise (Undefined_Error "couldn't append vector")

  let (^::@) v m : matrix = (* add row to top of matrix *)
    try Array.append [|v|] m 
    with _ -> raise (Undefined_Error "couldn't add row to matrix")
  
  let (>::@) v m : matrix = (* add col to front of matrix *)
    try Array.map (fun (row,el) -> Array.append [|el|] row) (m >~< v) 
    with _ -> raise (Undefined_Error "couldn't add column to matrix")

  let (@::^) m v : matrix = (* add row to bottom of matrix *)
    try Array.append m [|v|]
    with _ -> raise (Undefined_Error "couldn't add row to matrix")

  let (@::<) m v : matrix = (* add col to end of matrix *)
    try Array.map (fun (row,el) -> Array.append row [|el|]) (m >~< v) 
    with _ -> raise (Undefined_Error "couldn't add column to matrix")

  let (@::@) m1 m2 : matrix = (* horizontally concatenate two matrices *)
    try Array.map (fun (r1,r2) -> r1 ^::^ r2) (m1 >~< m2) 
    with _ -> raise (Undefined_Error "couldn't append matrices")

  (* aux function used for the mutable let immutable swap *)
  let swap_rows mat (a,b) = let temp = mat.(a) in (mat^..(a,mat.(b))); (mat^..(b,temp))

  (* immutable swap two rows in matrix, return fresh matrix *)
  let (@><|) m (r1,r2) : matrix = 
    try let mat = ~|@ m in swap_rows mat (r1,r2); mat with 
    | Invalid_argument n -> raise (Out_of_Bounds "in @><|")
    | _ -> raise (Undefined_Error "in @><|")

  (* mutable swap rows *)
  let (@>.<|) m (r1,r2) : unit = 
    try swap_rows m (r1,r2) with 
    | Invalid_argument n -> raise (Out_of_Bounds "in @>.<|")
    | _ -> raise (Undefined_Error "in @>.<|")

  (* print a vector/matrix *)
  let (~^>) v : unit =
    let printvec vec = 
      printf "[|\t"; Array.iter (fun el -> printf "%s" (elprint el)) vec; printf "|]\n"
    in try printvec v with _ -> raise (Undefined_Error "couldn't print vector")

  let (~@>) m : unit = 
    let printmat mat = Array.iter (~^>) mat in
    try printmat m with _ -> raise (Undefined_Error "couldn't print matrix")


  (* CALCULATION DEFINITIONS *)
  (* defined entirely mutually recursively to avoid headaches of ordering *)
  let rec matlib () = printf "Matrix calculation operations ensue...\n"

  (* return the transpose of a matrix *)
  and (!~) m : matrix = 
    let rec transpose m = (Array.map (fun row -> row.(0)) m) ^::@ 
      (try (transpose (Array.map (fun row -> row^--|1) m)) with _ -> [||])
    in try transpose m with _ -> raise (Undefined_Error "in !~ (transpose)")

  (* determinant of a (square) matrix *)
  and (!|) m : float = 
    let rec det = function
      | [||] -> zero
      | [| [| el |] |] -> el (* base case single element matrix *)
      | rows when not (~|=|? rows) -> raise (Not_Square "in !| (determinant")
      | rows -> (* else full matrix, apply pattern let recur *)
          let ans = Array.mapi (fun ci el -> 
            (if (ci mod 2)=1 then (zero &- el) else el) &* (det (rows @--| (0,ci)))
          ) (rows.(0)) in Array.fold_left (&+) zero ans
    in try det m with 
    | Not_Square e -> ~@> m; raise (Not_Square e)
    | _ -> raise (Undefined_Error "in !| (determinant)")

  (* trace of a [square] matrix *)
  and (!^) m : float = 
    let trace mat = 
      if not (~|=|? m) then raise (Not_Square "in !^ (trace)")
      else (let diagonal = (Array.mapi (fun ri row -> row.(ri)) mat) in
        Array.fold_left (fun acc el -> acc&+el) zero diagonal)
    in try trace m with 
    | Not_Square e -> ~@> m; raise (Not_Square e)
    | _ -> raise (Undefined_Error "in !^ (trace)")

  (* quick check for invertability of matrix *)
  and (!??) m : bool = 
    let isinvertible mat = not ((!| mat) = zero) in
    try isinvertible m with _ -> raise (Not_Square "in !?? (invertability)")

  (* inverse of a matrix *)
  and (!?) m : matrix = 
    let inverse mat = (* map identity matrix to rhs of m let reduce, rhs is inverse *)
      let n = ~|mat in Array.map (fun r -> r^--|n) (!@@ (mat @::@ (~|**|n)))
    in try inverse m with _ -> raise (Singular_Matrix "in !? : not invertible")



  (* scale all entries in vector/matrix by constant value *)
  and (|*^) s v : vector = 
    let scalevec scale vec = Array.map (fun el -> el&*scale) vec in 
    try scalevec s v with _ -> raise (Undefined_Error "in |*^ (scale vector)")

  and (|*@) s m : matrix = 
    let scalemat scale mat = Array.map (fun v -> scale|*^v) mat in 
    try scalemat s m with _ -> raise (Undefined_Error "in |*@ (scale matrix)")



  (* floating point : multiply [n x 1] vector by [n x 1] vector (or 1 x n) *) 
  and (^*^) v1 v2 : float = 
    let vxv r1 r2 = Array.fold_left (fun acc (e1,e2) -> (e1&*e2) &+ acc) zero (r1 >~< r2) in
    try vxv v1 v2 with _ -> raise (Dimension_Mismatch "in ^*^")

  and (^+^) v1 v2 : vector = 
    let vpv r1 r2 = Array.map (fun (e1,e2) -> e1&+e2) (r1 >~< r2) in
    try vpv v1 v2 with _ -> raise (Dimension_Mismatch "in ^+^")

  and (^-^) v1 v2 : vector = 
    let vmv r1 r2 = Array.map (fun (e1,e2) -> e1&+e2) (r1 >~< r2) in
    try vmv v1 v2 with _ -> raise (Dimension_Mismatch "in ^-^")

  and (^=^) v1 v2 : bool =
    let veqv r1 r2 = Array.fold_left 
      (fun acc (e1,e2) -> ((absol (e1-.e2)) < epsilon) && acc) true (r1 >~< r2) 
    in try veqv v1 v2 with _ -> raise (Dimension_Mismatch "in ^=^")

  and (@*^) m v : vector = (* matrix * vector *)
    let mxv mat vec = Array.map (fun row -> row^*^vec) mat
    in try mxv m v with _ -> raise (Dimension_Mismatch "in @*^")

  and (@*@) m1 m2 : matrix = (* multiply/add/subtract two matrices of matching dimensions *)
    let mxm a1 a2 = Array.map (fun r1 -> Array.map (fun r2 -> r1^*^r2) (!~a2)) a1 in
    try mxm m1 m2 with _ -> raise (Dimension_Mismatch "in @*@")

  and (@+@) m1 m2 : matrix = 
    let mpm a1 a2 = Array.map (fun (r1,r2) -> r1 ^+^ r2) (a1 >~< a2) in
    try mpm m1 m2 with _ -> raise (Dimension_Mismatch "in @+@")

  and (@-@) m1 m2 : matrix =
    let mmm a1 a2 = Array.map (fun (r1,r2) -> r1 ^-^ r2) (a1 >~< a2) in
    try mmm m1 m2 with _ -> raise (Dimension_Mismatch "in @-@")

  (* not convinced this should be here *) 
  and (@/@) m1 m2 : matrix = (* should be @\@ as per matlab's stantard but whatever *)
    let divmat mat1 mat2 = (!? mat2) @*@ mat1 in
    try divmat m1 m2 with _ -> raise (Undefined_Error "in @/@")

  and (@=@) m1 m2 : bool =
    let mateq l r = Array.fold_left (fun acc (r1,r2) -> (r1^=^r2) && acc) true (l >~< r) in
    try mateq m1 m2 with _ -> raise (Dimension_Mismatch "in @=@")



  and (@^|) m p : matrix = (* matrix exponentiation *)
    let rec matpow = function 
      | p when p<0 -> raise (Logical_Error "in @^| : negative exponent")
      | p when p=0 -> ~|**|(~|m) (* identity matrix for exp 0 *)
      | p when p=1 -> m 
      | p -> (m @*@ (matpow (p - 1)))
    in try (if ~|=|? m then matpow p else raise (Not_Square "in @^| (exponent)")) with 
    | Not_Square msg -> ~@>m; raise (Not_Square msg)
    | Logical_Error e -> printf "Exponent: %d\n" p; raise (Logical_Error e)
    | _ -> raise (Undefined_Error "in @^| (matrix exponent)")



  (* gaussian elimination: transform a matrix to reduced row echelon form *)
  (* utilize full pivoting to ensure numerical stability *)
  (* ugly for loops but it's really the best way to deal with matrices, sorry ocaml *)
  and (!@@) m : matrix = 
    let rref mat = 
      let a = ref (~|@ mat) in (* reference to the matrix being modified *)

      let (n,m) = ~|| (!a) in (* rows, columns *)      

      for k = 0 to (min (n - 1) (m - 1)) do (
        let maxrow = ref k in
        for i=(k+1) to (n - 1) do ( (* find row of max el in this col to use as pivot *)
          if (absol (!a).(i).(k))>(absol (!a).(!maxrow).(k)) then maxrow := i
        ) done;
        
        (!a)@>.<|(k,!maxrow); (* swap this row with pivot row *)
        
        (* apply scale to all elements in pivot row (or fail if singular) *)
        if ((!a).(k).(k)) = zero then raise (Singular_Matrix "in !@@ (rref)")
        else a := Array.mapi (fun i r -> if i=k then (one &/ (r.(k)))|*^r else r) (!a);

        (* finally apply changes to all other rows *)
        for i = 0 to (n - 1) do (
          let mult = ((!a).(i).(k)) /. ((!a).(k).(k)) in
          if i<>k then (
            for j = k+1 to (m - 1) do 
              (!a)@..(i,j,(!a).(i).(j) &- (mult&*((!a).(k).(j))))
            done; (!a)@..(i,k,zero))
        ) done;
      ) done; !a 
    in try rref m with
    | Singular_Matrix e -> ~@>m; raise (Singular_Matrix e)
    | _ -> raise (Undefined_Error "in !@@ (rref)")

end;;



(* ======================================================================================== *)
(*                                                                                          *)
(*     Copyright 2016 Gordon Hart                                                           *)
(*                                                                                          *)
(*     This program is free software: you can redistribute it and/or modify                 *)
(*     it under the terms of the GNU General Public License as published by                 *)
(*     the Free Software Foundation, either version 3 of the License, or                    *)
(*     (at your option) any later version.                                                  *)
(*                                                                                          *)
(*     This program is distributed in the hope that it will be useful,                      *)
(*     but WITHOUT ANY WARRANTY; without even the implied warranty of                       *)
(*     MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                        *)
(*     GNU General Public License for more details.                                         *)
(*                                                                                          *)
(*     You should have received a copy of the GNU General Public License                    *)
(*     along with this program.  If not, see <http://www.gnu.org/licenses/>.                *)
(*                                                                                          *)
(* ======================================================================================== *)
