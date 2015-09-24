open AST
open Error
open Env

let rec eval_expr env expr =
  match expr.edesc with 
  (*opertion: not, neg *)
  | Call(e,"not",[]) ->
    begin 
    match (eval_expr env e) with
    | Boolean v -> Boolean (not v)
    | _ -> Null (*express like if , assign returns Null type. *) 
    end
  | Call(e,"neg",[]) ->
    begin
    match (eval_expr env e) with
    | Int v -> Int(-v)
    | _ -> Null
    end
  (*operation: sub, add, mul, div, mod, and, or, gt, ge, lt, le, eq, neq*)
  | Call(e1, "sub", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) -> Int (a-b)
    | _ -> Null
    end
  | Call(e1, "add", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) -> Int (a+b)
    | _ -> Null
    end
  | Call(e1, "mul", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) -> Int (a*b)
    | _ -> Null
    end
  | Call(e1, "div", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) -> Int (a/b)
    | _ -> Null
    end
  | Call(e1, "mod", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) -> Int (a mod b)
    | _ -> Null
    end
  | Call(e1, "and", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Boolean a, Boolean b) -> Boolean (a && b)
    | _ -> Null
    end
  | Call(e1, "or", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Boolean a, Boolean b) -> Boolean (a || b)
    | _ -> Null
    end
  | Call(e1, "gt", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Boolean a, Boolean b) -> Boolean (a > b)
    | _ -> Null
    end
  | Call(e1, "ge", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Boolean a, Boolean b) -> Boolean (a >= b)
    | _ -> Null
    end
  | Call(e1, "lt", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Boolean a, Boolean b) -> Boolean (a < b)
    | _ -> Null
    end
  | Call(e1, "le", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Boolean a, Boolean b) -> Boolean (a <= b)
    | _ -> Null
    end
  | Call(e1, "eq", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) ->         Boolean (a=b)
    | (Boolean a, Boolean b) -> Boolean (a=b)
    | (String a, String b) ->   Boolean ((String.compare a b) = 0)
    | (Null, Null) ->           Boolean (true)
    | _ -> Null
    end
  | Call(e1, "neq", e2::_) -> 
    begin 
    match ((eval_expr env e1), (eval_expr env e2)) with
    | (Int a, Int b) -> Boolean (a<>b)
    | (Boolean a, Boolean b) -> Boolean(a<>b)
    | (String a, String b) -> Boolean((String.compare a b) <> 0)
    | (Null, Null) -> Boolean (true)
    | _ -> Null
    end
  | New t ->
    Null
  | Seq (e1,e2) -> 
    eval_expr env e1;
    eval_expr env e2
  | If (e0, e1, e2) ->
    begin 
    match (eval_expr env e0) with 
    | Boolean true -> (eval_expr env e1)
    | Boolean false -> (eval_expr env e2)
    end
  | Val v ->
    begin
    match v with
    | String s -> String (s)
    | Int i -> Int (i)
    | Boolean b -> Boolean (b)
    | Null -> Null
    end