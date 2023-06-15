type instruction = Eval of Term.term | Assign of (string * Term.term)
type program = instruction list

module VarTable = Map.Make (String)

let string_of_instruction = function
  | Eval t -> Term.string_of_term t ^ ";"
  | Assign (s, t) -> s ^ " := " ^ Term.string_of_term t ^ ";"

let string_of_program (p : program) =
  let rec aux p acc =
    match p with
    | [] -> acc
    | i :: p -> aux p (acc ^ string_of_instruction i ^ "\n")
  in
  aux p ""

let rec eval_term t vars =
  match t with
  | Term.Int a -> a
  | Term.Var s -> VarTable.find s vars
  | Term.Add (t1, t2) -> eval_term t1 vars + eval_term t2 vars
  | Term.Sub (t1, t2) -> eval_term t1 vars - eval_term t2 vars
  | Term.Mul (t1, t2) -> eval_term t1 vars * eval_term t2 vars
  | Term.Div (t1, t2) -> eval_term t1 vars / eval_term t2 vars
  | Term.Mod (t1, t2) -> eval_term t1 vars mod eval_term t2 vars

let execute_program program =
  let execute_instruction i vars =
    match i with
    | Eval t ->
        let res = eval_term t vars in
        Printf.printf "%d\n" res;
        vars
    | Assign (s, t) ->
        let res = eval_term t vars in
        Printf.printf "%s := %d\n" s res;
        VarTable.add s res vars
  in

  let rec exec vars = function
    | [] -> ()
    | i :: p ->
        let vars = execute_instruction i vars in
        exec vars p
  in

  exec VarTable.empty program