# An OCaml Matrix Library

Purely symbolic matrix manipulation suite comprised of **45 operators** for matrix (OCaml `array`) creation, manipulation and calculation——mostly translations of Array library functions with some interesting additions. Will likely continue to grow as it is used.

Implemented using native floats but can be easily changed by altering the block defining literals:
```
  type vector = float array 

  let zero = 0. and one = 1. 
  and absol = fun x -> abs_float x
  and elprint = fun e -> sprintf "%0.3f\t" e
  and epsilon = 1e-5
  and (&+) a b = a+.b
  and (&-) a b = a-.b
  and (&* ) a b = a*.b
  and (&/) a b = a/.b
```


### Usage

In general, the leftmost char of an operator signals the associativity/left argument for the prefix or infix operators. The middle character denotes the operation being carried out, and the rightmost character signals the righthand argument for infix operators.
```
(*     ~      prefix - if first char of op is ~ then there is only 1 argument, postfixed    *)
(*     !      prefix - same as ~ but for matrix operations                                  *)
(*     ^      vector - whenever this shows up, the argument on that side is a vector        *)
(*     @      matrix - same as ^ but for matrices                                           *)
(*     |      scalar - same as ^,@, used when a scalar is required as an argument           *)
(*                                                                                          *)
(*     ~?>    print operator for matrix/vector as described by ?                            *)
```