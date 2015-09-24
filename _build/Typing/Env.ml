open Error
exception ClassAlreadyDefined of string
exception MethodAlreadyDefined of string
exception AttributeAlreadyDefined of string
exception ExtendsException of string
exception ClassNotDefined of string
exception ParentNotDefined of string
exception AttributeNotDefined of string
exception VariableNotDefined of string
exception MethodNotDefined of string

(*
  Env store class list (typed_cl), all their attributes (typed_attrs) 
  and methods (typed_meths)
  typed_local_vars is used to store local variables, and will be 
  refreshed each time when we type a meth;
  when a variable appears, we search firstly inside typed_local_vars, then in 
  typed_attrs to see if this variable exits
*)
type 
typed_cl = (string, typedC) Hashtbl.t 
and 
typed_attrs = (string, Type.t) Hashtbl.t 
and 
typed_meths = (string, typedM) Hashtbl.t
and 
typed_local_vars = (string, Type.t) Hashtbl.t
and
typedC = {
  name: string;
  mutable parent : Type.t;
  meths : typed_meths;
  attrs : typed_attrs;
}
and
typedM = {
  meth_args : typedArg list ;
  meth_return : Type.t;
}
and 
typedEnv = {
  env_cl : typed_cl;
  mutable env_vl : typed_local_vars;
}
and 
typedArg = string * Type.t

let makeEnv cl = {
  env_cl = cl;
  env_vl = Hashtbl.create 11;
}

(* type class: basic class, Object, Int,  Boolean *)  
let typeClass cname= {
  name = cname;
  (*to simplify the problem, we make Object as the parent of Object itself*)
  parent = Type.fromString "Object";
  meths = Hashtbl.create 11;
  attrs = Hashtbl.create 11;
}

let typeClassObject =
  let c = typeClass "Object" in
    let typ_obj = Type.fromString "Object" in
      let typ_bool = Type.fromString "Boolean" in
        Hashtbl.add c.meths "eq"  { meth_args = [("", typ_obj)]; meth_return = typ_bool }; 
        Hashtbl.add c.meths "neq" { meth_args = [("", typ_obj)]; meth_return = typ_bool };
  c
  
let typeClassInt =
  let c = typeClass "Int" in
    let typ_int = Type.fromString "Int" in
      let typ_bool = Type.fromString "Boolean" in
        Hashtbl.add c.meths "sub" { meth_args = [("",typ_int)]; meth_return = typ_int };
        Hashtbl.add c.meths "add" { meth_args = [("",typ_int)]; meth_return = typ_int };
        Hashtbl.add c.meths "mul" { meth_args = [("",typ_int)]; meth_return = typ_int };
        Hashtbl.add c.meths "div" { meth_args = [("",typ_int)]; meth_return = typ_int };
        Hashtbl.add c.meths "mod" { meth_args = [("",typ_int)]; meth_return = typ_int };
        Hashtbl.add c.meths "neg" { meth_args = [];             meth_return = typ_int };
        Hashtbl.add c.meths "gt"  { meth_args = [("",typ_int)]; meth_return = typ_bool };
        Hashtbl.add c.meths "ge"  { meth_args = [("",typ_int)]; meth_return = typ_bool };
        Hashtbl.add c.meths "lt"  { meth_args = [("",typ_int)]; meth_return = typ_bool };
        Hashtbl.add c.meths "le"  { meth_args = [("",typ_int)]; meth_return = typ_bool };
  c
  
let typeClassBool =
  let c = typeClass "Boolean" in
    let typ_bool = Type.fromString "Boolean" in
      Hashtbl.add c.meths "and"  { meth_args = [("", typ_bool)]; meth_return = typ_bool };
      Hashtbl.add c.meths "or"   { meth_args = [("", typ_bool)]; meth_return = typ_bool };
      Hashtbl.add c.meths "not"  { meth_args = [];               meth_return = typ_bool };
  c

let initialEnv () = 
  let cl=Hashtbl.create 11 in
    let typedInitialEnv = makeEnv cl in
      Hashtbl.add typedInitialEnv.env_cl "Object"   typeClassObject; (* hash.add tablename key saval *)
      Hashtbl.add typedInitialEnv.env_cl "Int"      typeClassInt;
      Hashtbl.add typedInitialEnv.env_cl "Boolean"  typeClassBool;
      Hashtbl.add typedInitialEnv.env_cl "String"   (typeClass "String"); (* no methodes inside *)
      Hashtbl.add typedInitialEnv.env_cl "Null"     (typeClass "Null"); (* no methodes inside *)
  typedInitialEnv

(* find elements/ check element exists inside ENV*)
let findClass env cname = 
  try Hashtbl.find env.env_cl cname
  with Not_found -> raise (ClassNotDefined(cname)) 

let isClass env cname = 
  try 
    findClass env cname; true
  with ClassNotDefined _ -> false
  
let rec findMeth env cname mname =
  try 
    let c = findClass env cname in
      try 
        Hashtbl.find c.meths mname
      with Not_found ->
        if cname = "Object" 
        then raise (MethodNotDefined(mname)) 
        else 
        begin (* see if parent contains methode *)
          let pname = Type.stringOf c.parent in
            findMeth env pname mname
        end
  with ClassNotDefined _ -> raise (MethodNotDefined(mname))

let rec isMeth env cname mname =
  try 
    findMeth env cname mname; true
  with MethodNotDefined _-> false

let rec findVar env c_name vname=
  match c_name with
  | None -> (*expression outside of class*)
    begin
    try 
    Hashtbl.find env.env_vl vname
    with Not_found -> raise (VariableNotDefined(vname))
    end
  | Some cname ->
    begin
    try 
      Hashtbl.find env.env_vl vname
    with Not_found ->
      begin
      try 
        let c = findClass env cname in
          try 
            Hashtbl.find c.attrs vname
          with Not_found -> 
            if cname="Object" then
              raise (VariableNotDefined(vname))
            else 
            begin (* see if parent contains variable(attribute) *)
              let pname = Type.stringOf c.parent in
                findVar env (Some pname) vname
            end
      with ClassNotDefined _ -> raise (VariableNotDefined(vname))
      end
    end

let rec isVar env c_name vname=
  try 
    findVar env c_name vname; true
  with VariableNotDefined _ -> false

let rec isAttr env cname aname = (* check if aname already defined inside global env*)
  try 
    let c = findClass env cname in
      try 
        Hashtbl.find c.attrs aname; true
      with Not_found ->
        if cname="Object" then
          false
        else 
        begin (* see if parent contains attribute *)
          let pname = Type.stringOf c.parent in
            isAttr env pname aname
        end
  with ClassNotDefined _ -> false


(* add elements to ENV*)
let addClass env cname =
  if (isClass env cname) 
  then raise (ClassAlreadyDefined(cname))
  else
  let c_class= typeClass cname in
  Hashtbl.add env.env_cl cname c_class; 
  env
  
let addAttr env cname aname aType = 
  if (isAttr env cname aname) then raise (AttributeAlreadyDefined(aname)) ;
  try 
    let c = findClass env cname in
      Hashtbl.add c.attrs aname aType;
      Hashtbl.add env.env_cl cname c;
      makeEnv env.env_cl
  with ClassNotDefined _ -> raise (AttributeAlreadyDefined(aname)) 

let rec addLocalVar hashtb args =
  match args with
  | [] -> hashtb
  | (argname, argtype) ::rest ->
    Hashtbl.add hashtb argname argtype;
    addLocalVar hashtb rest

let refreshLocalVarsEnv env args =
  Hashtbl.reset env.env_vl; (*create local variable environment*)
  env.env_vl <- (addLocalVar env.env_vl args)
  
let addMeth env cname mname typedMeth =
  if (isMeth env cname mname) 
  then raise (MethodAlreadyDefined(mname))
  else
  try
    let c = findClass env cname in
      Hashtbl.add c.meths mname typedMeth;
      Hashtbl.add env.env_cl cname c;
      makeEnv env.env_cl
  with ClassNotDefined _ -> raise (MethodAlreadyDefined(mname)) 
  
(*check if extends cycle exists*)
let rec isChildOf env lname rname=
  if lname=rname then true
  else 
  begin
    if lname = "Object" 
    then false (*Object can't be subtypeof any class*)
    else
    try 
      let c_class = findClass env lname in
      let p_of_c = c_class.parent in
      isChildOf env (Type.stringOf p_of_c) rname
    with ClassNotDefined _ -> false
  end

let setParent env cname pname =
  try 
    let c = findClass env cname in
      if not (isChildOf env pname cname) 
      then
        begin
          c.parent <- Type.fromString pname;
          Hashtbl.replace env.env_cl cname c;
          env
        end
      else raise (ExtendsException(cname)) (*ExtendsCycleDetected*)
  with ClassNotDefined _ -> raise (ExtendsException(cname)) (*simplify the code*)
  
  


