open Error
open AST
open Env

(*some useful functions to get name and type from locatedType*)
let type_of_loctyp loactedType=
  Located.elem_of loactedType
  
let name_of_loctyp loactedType=
  Type.stringOf (Located.elem_of loactedType)

let type_of_typed_expr envExpr=
  match envExpr.etype with 
  | None -> typing_error envExpr.eloc
  | Some t -> t

(*check if ltype isSubTypeOf rtype*)
let rec isSubTypeOf env ltype rtype=    
  let lname = Type.stringOf ltype in 
  let rname = Type.stringOf rtype in
  if lname=rname then true
  else 
  begin
    if lname = "Object" 
    then false (*Object can't be subtypeof any class*)
    else
    try 
      let c_class = Env.findClass env lname in
      let p_of_c = c_class.parent in
      isSubTypeOf env p_of_c rtype
    with ClassNotDefined _ -> false
  end

(*=======start type expression =========*)
(* astExpr: AST  format without etype, expr_typer gives astExpr its type  *)
let rec expr_typer env c_name astExpr=  
  (*print_endline (string_of_expression_desc astExpr.edesc); *)
  match astExpr.edesc with 
  | New t -> 
    let typname=name_of_loctyp t in
    if not (Env.isClass env typname)
    then type_not_defined typname astExpr.eloc
    else astExpr.etype <- Some (type_of_loctyp t)
  | Seq (e1,e2) -> 
    expr_typer env c_name e1;
    expr_typer env c_name e2;
    astExpr.etype <- e2.etype (* take second expression as final type*)
  (* Type of If is Null *)
  | If (e0, e1, e2) ->
    expr_typer env c_name e0;
    let e0type = type_of_typed_expr e0 in
    if (Type.stringOf e0type) <> "Boolean"
	  then not_same_type (Type.stringOf e0type) "Boolean" e0.eloc
    expr_typer env c_name e1;
    expr_typer env c_name e2; 
    astExpr.etype <- Some (Type.fromString "Null")
  | Val v -> 
    begin
      match v with
      | String s -> 
        astExpr.etype <- Some (Type.fromString "String")
      | Int i -> 
        astExpr.etype <- Some (Type.fromString "Int")
      | Boolean b -> 
        astExpr.etype <- Some (Type.fromString "Boolean")
      | Null -> 
        astExpr.etype <- Some (Type.fromString "Null")
    end
  | Var vname ->
    begin
      try 
        astExpr.etype <- Some (Env.findVar env c_name vname)
      with VariableNotDefined _ -> type_not_defined vname astExpr.eloc
    end
  (* Type of Assign is Null *)
  | Assign (vname,e1) -> 
    begin
    try 
      let vtype = (Env.findVar env c_name vname) in
	    expr_typer env c_name e1;
	    let e1type = type_of_typed_expr e1 in
	    (*if vtype is parent of etype, then correct*)
      if (isSubTypeOf env e1type vtype)
      then astExpr.etype <- Some (Type.fromString "Null")
      else not_same_type (Type.stringOf vtype) (Type.stringOf e1type) astExpr.eloc
    with VariableNotDefined _ -> type_not_defined vname astExpr.eloc
    end
  | Define (lname,utype,e1,e2) ->
    if (Env.isVar env c_name lname)
    then type_already_defined lname astExpr.eloc
    else
      let utypename = name_of_loctyp utype in
      if not (isClass env utypename)
      then type_not_defined utypename astExpr.eloc;
      (*add lname to local variable environment*)
      Env.addLocalVar env.env_vl [(lname, type_of_loctyp utype)];
      expr_typer env c_name e1;
      let e1type = type_of_typed_expr e1 in
      (* e1type should be child of utype, then OK *)
  	  if not (isSubTypeOf env e1type (type_of_loctyp utype))
  	  then not_same_type utypename (Type.stringOf e1type) astExpr.eloc;
      expr_typer env c_name e2;
      astExpr.etype <- Some (Type.fromString "Null")
  | Cast (ctype,e1) -> 
    let ctypename= name_of_loctyp ctype in
    if not (isClass env ctypename)
    then type_not_defined ctypename astExpr.eloc;
    expr_typer env c_name e1;
    let e1type = type_of_typed_expr e1 in
    (* ctype should be child of etype *)
    if (isSubTypeOf env (type_of_loctyp ctype) e1type)
    then astExpr.etype <- Some (type_of_loctyp ctype)
    else not_castable (Type.stringOf e1type) ctypename astExpr.eloc
  | Instanceof (e1,tpy) -> 
    let tpyname= name_of_loctyp tpy in
    if not (isClass env tpyname)
    then type_not_defined tpyname astExpr.eloc;
    expr_typer env c_name e1;
    begin
    match e1.etype with
    | None -> typing_error e1.eloc
    | Some t ->
      astExpr.etype <- Some (Type.fromString "Boolean")
    end
  | Call (e1,op,mparams) -> 
    begin
      (* 2 things to check here: 
        1. check if UIDENT of e1 contains method op;
        2. check if mparams list is relative to argumets list
      *)
      expr_typer env c_name e1;
      let e1type = type_of_typed_expr e1 in
      if not (isMeth env (Type.stringOf e1type) op)
      then type_not_defined op astExpr.eloc;
      let c_meths=findMeth env (Type.stringOf e1type) op in
      compareParArgList env c_name c_meths.meth_args mparams astExpr.eloc;
      astExpr.etype <- Some (c_meths.meth_return)
    end
  | _ -> expression_incorrect astExpr.eloc

and compareParArgList env c_name largs lparams loc=
  match (largs, lparams) with
  |([], []) -> ()                                                                                                                                                                                                                                                                                                                                                                                                           
  |([], lp) -> too_many_params loc
  |(la, []) -> not_enough_params loc
  |( (aname, atype)::las,  p::lps) ->
    begin
      expr_typer env c_name p; 
      let ptype = type_of_typed_expr p in
      if (Type.stringOf ptype)=(Type.stringOf atype)
      then compareParArgList env c_name las lps loc
      else
      begin
        (* if ptype is child of atype, then OK *)
        if (isSubTypeOf env ptype atype)
        then compareParArgList env c_name las lps loc
        else params_error loc
      end
    end
(*=======end type expression =========*)