type term =
  | Int of int
  | Var of string
  | Add of term * term
  | Sub of term * term
  | Mul of term * term
  | Div of term * term
  | Mod of term * term

let rec string_of_term = function
  | Int a -> string_of_int a
  | Var s -> s
  | Add (t1, t2) -> "(" ^ string_of_term t1 ^ ") + (" ^ string_of_term t2 ^ ")"
  | Sub (t1, t2) -> "(" ^ string_of_term t1 ^ ") - (" ^ string_of_term t2 ^ ")"
  | Mul (t1, t2) -> "(" ^ string_of_term t1 ^ ") × (" ^ string_of_term t2 ^ ")"
  | Div (t1, t2) -> "(" ^ string_of_term t1 ^ ") / (" ^ string_of_term t2 ^ ")"
  | Mod (t1, t2) ->
      "(" ^ string_of_term t1 ^ ") mod (" ^ string_of_term t2 ^ ")"

let rec simplify_term =
  let aux = function
    | Add (Int a, Int b) -> Int (a + b)
    | Sub (Int a, Int b) -> Int (a - b)
    | Mul (Int a, Int b) -> Int (a * b)
    | Div (Int a, Int b) -> Int (a / b)
    | Mod (Int a, Int b) -> Int (a mod b)
    | t -> t
  in

  function
  | Add (t1, t2) -> aux (Add (simplify_term t1, simplify_term t2))
  | Sub (t1, t2) -> aux (Sub (simplify_term t1, simplify_term t2))
  | Mul (t1, t2) -> aux (Mul (simplify_term t1, simplify_term t2))
  | Div (t1, t2) -> aux (Div (simplify_term t1, simplify_term t2))
  | Mod (t1, t2) -> aux (Mod (simplify_term t1, simplify_term t2))
  | t -> t
