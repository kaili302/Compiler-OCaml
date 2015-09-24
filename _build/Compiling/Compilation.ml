open EnvHeap
open AST

let rec addMethsToClass env cname meths=
  match meths with 
  | [] -> env
  | meth :: rest -> 
    let env1 = addMeth env cname meth.mname meth in
    addMethsToClass env1 cname rest

let rec addAttrsToClass env cname attrs=
  match attrs with 
  | [] -> env
  | attr :: rest ->
    let env1 = addAttr env cname attr.aname attr.adefault in
    addAttrsToClass env1 cname rest

let rec addClassesToEnv env cl =
  match cl with 
  | [] -> env
  | c :: rest ->
    let env_c = addClass env c.cname in 
    let pname = Type.stringOf (Located.elem_of c.cparent) in
    let env_c_p = setParent env_c c.cname pname in 
    let env_c_p_a = addAttrsToClass env_c_p c.cname c.cattributes in
    addMethsToClass env_c_p_a c.cname c.cmethods
    
let compile cl =
  addClassesToEnv (initialEnv ()) cl
  