(* The error type with its associated exception for the compiler *)
type t
exception Error of t * Location.t

(* print an error *)
val report_error : t -> unit

(* raise the various errors *)
val illegal_char : char -> Location.t -> 'a
val illegal_escape_char : Location.t -> 'a
val unterminated_string : Location.t -> 'a
val unterminated_comment : Location.t -> 'a
val syntax : Location.t -> 'a
val type_already_defined : string -> Location.t -> 'a
val type_not_defined : string -> Location.t -> 'a
val extends_error : string -> string -> Location.t -> 'a
val not_same_type : string -> string -> Location.t -> 'a
val meth_return_error : string -> string -> Location.t -> 'a
val typing_error : Location.t -> 'a
val not_castable : string -> string -> Location.t -> 'a
val too_many_params : Location.t -> 'a
val not_enough_params : Location.t -> 'a
val params_error : Location.t -> 'a
val expression_incorrect : Location.t -> 'a


