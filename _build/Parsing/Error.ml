open Location

type t =
  | Illegal_character of char
  | Illegal_escape_char
  | Unterminated_string
  | Unterminated_comment
  | Syntax
  | TypeAlreadyDefined of string
  | Typingerror 
  | TypeNotDefined of string
  | ExtendsError of string * string
  | NotSameType of string * string
  | MethodReturnError of string * string
  | NotCastable of string * string
  | TooManyParams
  | NotEnoughParams
  | ParameterError
  | ExpressionError

exception Error of t * Location.t

(* Les erreurs. *)
let report_error = function
  | Illegal_character c ->
      print_string "Illegal character (";
      print_char c;
      print_string "): "
  | Illegal_escape_char ->
      print_endline "Illegal escape character in string: "
  | Unterminated_string ->
      print_endline "String literal not terminated: "
  | Unterminated_comment ->
      print_endline "Comment not terminated: "
  | Syntax ->
      print_endline "Syntax error: "
  | TypeAlreadyDefined t ->
    print_string ("Type (class or method or attribute or argument) already defined "^t^": ")
  | Typingerror ->
      print_string ("An error occured while typing")
  | TypeNotDefined t ->
    print_string ("Type (class or method or attribute or argument) not defined "^t^": ")
  | ExtendsError (cn, pn) ->
    print_string ("Extends error detected between "^cn^" and "^pn^": ")
  | NotSameType (lname,rname) ->
      print_string (lname^" can't be defined as type "^rname^": ")
  | MethodReturnError (mtype, btype) ->
      print_string ("Method needs "^btype^ " as return type, but "^mtype^" is given:")
  | NotCastable(et,ct) ->
      print_string ("Can't cast "^et^" to "^ct^": ")   
  | TooManyParams ->
      print_string ("Too many parameters :")   
  | NotEnoughParams ->
      print_string ("Not enough parameters :")  
  | ParameterError ->
      print_string ("Check your method parameter list: ")
  | ExpressionError ->
     print_string ("Incorrect expression found at : ")
    

let illegal_char char loc =
  raise (Error(Illegal_character char, loc))

let illegal_escape_char loc =
  raise (Error(Illegal_escape_char, loc))

let unterminated_string loc =
  raise (Error (Unterminated_string, loc))

let unterminated_comment loc =
  raise (Error (Unterminated_comment, loc))

let syntax loc =
  raise (Error (Syntax, loc))
  
  
let type_already_defined tname tloc =
  raise (Error(TypeAlreadyDefined tname, tloc))

let type_not_defined tname tloc =
  raise (Error(TypeNotDefined tname, tloc))

let extends_error cname pname cloc =
  raise (Error(ExtendsError (cname, pname), cloc))

let not_same_type lname rname loc =
  raise (Error(NotSameType (lname,rname),loc))
  
let meth_return_error mtype btype loc =
  raise (Error(MethodReturnError (mtype,btype),loc))

let typing_error loc =
  raise (Error(Typingerror, loc))

let not_castable etype ctype loc =
  raise (Error(NotCastable (etype,ctype),loc))
  
let too_many_params loc = 
  raise (Error(TooManyParams,loc))

let not_enough_params loc =
  raise (Error(NotEnoughParams,loc))
  
let params_error loc =
  raise (Error(ParameterError, loc))

let expression_incorrect loc =
  raise (Error(ExpressionError, loc))