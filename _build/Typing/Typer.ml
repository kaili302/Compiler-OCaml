open Error
open AST
open Type
open Env
open ExprTyper

(* ======begin type methods ==========*)
let rec methods_typer env c meths =
  match meths with 
    | [] -> env
    | m::rest -> 
      (*overload, override are  not allowed in our compilateur*)
      if (Env.isMeth env c.cname m.mname)
      then type_already_defined m.mname m.mloc
      (* now type the method body and the arguments *)
      (* 1. check method type; 
         2. check arguments type;  
         3. check body(expression) type;
         4. compare mtype and bodyType; Exception when mtype=Null but bodytype!=Null -> Null getName(){"name"} (error!!)
         5. add Method to env *)
      else  
        let mtypename=name_of_loctyp m.mreturntype in
        if not (Env.isClass env mtypename) 
        then type_not_defined mtypename m.mloc;
        (*check aruments list and remove location from type*)
        let args=checkMethArgs env [] m.margstype in
        (*reset then add arguments to local variable environment*)
        Env.refreshLocalVarsEnv env args;
        (*add etype to method body expression*)
        expr_typer env (Some c.cname) m.mbody;
        let mbodytype = type_of_typed_expr m.mbody in
        let mreturntype = type_of_loctyp m.mreturntype in
        (* if mbodytype = mreturntype || (mbodytype <> mreturntype && mbodytype issubtypeof mreturntype) OK*)
        if ((Type.stringOf mbodytype)=mtypename ||
        ( (Type.stringOf mbodytype)<>mtypename && isSubTypeOf env mbodytype mreturntype)) 
        then 
          let typedMeth={ meth_args = args; meth_return = mreturntype} in
          try 
            let env1 = Env.addMeth env c.cname m.mname typedMeth in
            methods_typer env1 c rest
          with MethodAlreadyDefined _ -> type_already_defined m.mname m.mloc
        else meth_return_error (Type.stringOf mbodytype) mtypename m.mloc
        
and checkMethArgs env args astLocArgs =
  match astLocArgs with
    | [] -> args
    | (argname, loctyp)::rest ->
      if not (Env.isClass env (name_of_loctyp loctyp))
      then type_not_defined (name_of_loctyp loctyp) (Located.loc_of loctyp)
      else 
      let new_args= (argname, (type_of_loctyp loctyp)) :: args in
      checkMethArgs env new_args rest

let rec begin_methods_typer env cl = 
  match cl with
  | [] -> env
  | c::rest ->
    let env1 = methods_typer env c c.cmethods in
      begin_methods_typer env1 rest
(* ======end type methods ==========*)

(* ======begin type attributes ==========*)
(* type all attributes inside class c*)
let rec attributes_typer env c al = 
  match al with
  | [] -> env
  | a::rest ->
    (* 3 things to check
      1. check if a.atype is already difined;
      2. check if a.aname is already used inside the class;
      3. check if a.adefault is subtype of a.atype
    *)
    let atypename = name_of_loctyp a.atype in
    let atypetype = type_of_loctyp a.atype in
    if not (Env.isClass env atypename)
    then type_not_defined atypename a.aloc
    else
    if (Env.isAttr env c.cname a.aname)
    then type_already_defined a.aname a.aloc;
    (* now type the attribute *)
    match a.adefault with
    | None -> 
      let env1 = Env.addAttr env c.cname a.aname atypetype in
      attributes_typer env1 c rest
    | Some e -> 
      
      expr_typer env (Some c.cname) e;
      let adefaulttype=type_of_typed_expr e in
      if ((Type.stringOf adefaulttype) = atypename) (*attribute defined and value type is the same*)
      then 
        let env1 = Env.addAttr env c.cname a.aname atypetype in
        attributes_typer env1 c rest
      else
      begin
        (*is adefaulttype is subtype of atype , ok*)
        if (isSubTypeOf env adefaulttype atypetype)
        then 
          let env1 = Env.addAttr env c.cname a.aname atypetype in
          attributes_typer env1 c rest
        else not_same_type atypename (Type.stringOf adefaulttype) a.aloc
      end

let rec begin_attributes_typer env cl = 
  match cl with
  | [] -> env
  | c::rest ->
    let env1 = attributes_typer env c c.cattributes in
      begin_attributes_typer env1 rest

(* ======end type attributes ==========*)

(* type parent *)
let rec parent_typer env cl= 
  match cl with (*cl: AST format*)
  | [] -> env
  | c::rest -> 
    let pname = name_of_loctyp c.cparent in
    
    (* case1: parent doesn't exist in ENV, error; 
       case2: parent exists (Object or self-defined), add to cl;
       keep ATTENTION of extends cycle*)
    if not (Env.isClass env pname) (* check case1 *)
    then type_not_defined pname c.cloc
    else 
    try
    parent_typer (setParent env c.cname pname) rest
    with ExtendsException _ -> extends_error c.cname pname c.cloc

(* type  class *)
let rec cl_typer env cl= 
  match cl with (* cl: AST format*)
  | [] -> env
  | c::rest -> 
    let env1 = Env.addClass env c.cname in
    try cl_typer env1 rest
    with ClassAlreadyDefined _ -> type_already_defined c.cname c.cloc

(* start typing AST of classes list *)
let ast_typer env cl=  
  let typedEnv1 = cl_typer env cl in (*type class*)
  let typedEnv2 = parent_typer typedEnv1 cl in (*type parent*)
  let typedEnv3 = begin_attributes_typer typedEnv2 cl in (*type attributes*)
  begin_methods_typer typedEnv3 cl (*type methods*)
  
let typer_start (cl,e_op) =
  let env=ast_typer (Env.initialEnv()) cl in
  match e_op with
  | None -> print_endline "  nop"
  | Some e -> 
    expr_typer env None e