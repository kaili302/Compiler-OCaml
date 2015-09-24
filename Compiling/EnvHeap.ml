(*mainly used when call a class's function*)
open AST
type
typed_cl = (string, typedC) Hashtbl.t 
and 
typedC = {
  name: string;
  mutable parent : string;
  attrs : (string, AST.expression_desc) Hashtbl.t;
  meths : (string, typedM) Hashtbl.t;
}
and
typedM = {
  args : string list ; (*local values list*)
  body : AST.expression_desc;
}
and
env = {
  env_cl : typed_cl;
  (*an env for local variables in method, each time a method is called, this env should be refreshed*)
  mutable env_vl : (string, AST.expression_desc) Hashtbl.t; 
  (*an env for local variables in main evaluate expression (expression outside class)*)
  env_vl_expr : (string, AST.expression_desc) Hashtbl.t; 
}

let makeEnv cl = {
  env_cl = cl;
  env_vl = Hashtbl.create 11;
  env_vl_expr = Hashtbl.create 11;
}

let makeClass cname = {
  name = cname;
  parent = "Object";
  attrs = Hashtbl.create 11;
  meths = Hashtbl.create 11;
}

let initialEnv () =
  makeEnv (Hashtbl.create 11)

let findClass env cname = 
  Hashtbl.find env.env_cl cname

let rec findMeth env cname mname =
  let c = findClass env cname in
    try 
      Hashtbl.find c.meths mname
    with Not_found ->
      findMeth env c.parent mname

let rec findVar env cname vname=
  let c = findClass env cname in
    Hashtbl.find c.attrs vname

(* add elements to ENV*)
let addClass env cname =
  Hashtbl.add env.env_cl cname (makeClass cname); 
  env
  
let setParent env cname pname =
  let c = findClass env cname in
  c.parent <- pname;
  Hashtbl.replace env.env_cl cname c;
  env
  
let addAttr env cname aname adefault = 
  let c = findClass env cname in
  begin
  match adefault with
  | Some v ->
    Hashtbl.add c.attrs aname v.edesc;
    Hashtbl.replace env.env_cl cname c;
    env
  | None ->
    Hashtbl.add c.attrs aname (Val(Null));
    Hashtbl.replace env.env_cl cname c;
    env
  end 
  
let rec getMethArgsName env args astLocArgs =
  match astLocArgs with
    | [] -> args
    | (argname, loctyp)::rest ->
      let new_args= argname :: args in
      getMethArgsName env new_args rest
  
let addMeth env cname mname astMeth =
  let c = findClass env cname in
  let argsname = getMethArgsName env [] astMeth.margstype in
  let meth = { args = argsname; body = astMeth.mbody.edesc } in
    Hashtbl.add c.meths mname meth;
    Hashtbl.replace env.env_cl cname c;
    env