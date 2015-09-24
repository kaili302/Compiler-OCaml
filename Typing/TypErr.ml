type t =
  | TypeAlreadyDefined of string
  | Typingerror 
  | TypeNotDefined of string
  | ExtendsCycle of string * string
  | NotSameType of string * string
  | MethodReturnError of string * string
  | Notcastable of string * string
  | ParameterError

exception Error of string * Location.t

let report_error = function
  | TypeAlreadyDefined t ->
    print_string ("Type (class or method or attribute or argument) already defined "^t^": ")
  | Typingerror ->
      print_string ("An error occured while typing")
  | TypeNotDefined t ->
    print_string ("Type (class or method or attribute or argument) not defined "^t^": ")
  | ExtendsCycle (cn, pn) ->
    print_string ("Extends cycle detected between "^cn^" and "^pn^": ")
  | NotSameType (lname,rname) ->
      print_string (lname^" can't be defined as type "^rname^": ")
  | MethodReturnError (mtype, btype) ->
      print_string ("Method needs "^mtype^ " as return type, but "^btype^" is given:")
  | Notcastable(et,ct) ->
      print_string ("Can't cast "^et^" to "^ct^": ")    
  | ParameterError ->
      print_string ("Check your method parameter list: ")
    

let type_already_defined tname tloc =
  raise (Error(TypeAlreadyDefined tname, tloc))

let type_not_defined tname tloc =
  raise (Error(TypeNotDefined tname, tloc))

let extends_cycle cname pname cloc =
  raise (Error(ExtendsCycle (cname, pname), cloc))

let not_same_type lname rname loc =
  raise (Error(NotSameType (lname,rname),loc))
  
let meth_return_error mtype btype loc =
  raise (Error(MethodReturnError (mtype,btype),loc))

let typing_error loc =
  raise (Error(Typingerror, loc))

let not_castable etype ctype loc =
  raise (Error(NotCastable (etype,ctype),loc))
  
let params_error loc =
  raise (Error(ParameterError, loc))
  