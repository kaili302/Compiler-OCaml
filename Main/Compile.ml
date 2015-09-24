let execute lexbuf verbose = 
  try 
    let ast = Parser.start Lexer.token lexbuf in
    print_endline "successfull parsing";
    if verbose then AST.print_program ast;
    print_endline "typing todo";
    Typer.typer_start ast;
    print_endline "typing finished";
    match ast with
    | (cl,Some expr) ->
        match expr.etype with
        | Some etypes ->
          let env = Compilation.compile cl in
          let result = ExprEval.eval_expr env expr in
            print_endline (AST.string_of_value result)
        | None ->
          print_endline "type error"
    | _ -> 
      print_endline "no evaluation"
  with 
    | Parser.Error ->
      print_string "Syntax error: ";
      Location.print (Location.curr lexbuf)
    | Error.Error(e,l) ->
      Error.report_error e;
      Location.print l
