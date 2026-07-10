open! Core

val are_models_equivalent : string -> string -> unit
val test : ?html_syntax_module:string -> string -> unit

(** Like [test], but expects the input to produce a parse error. Prints the error message
    readably (with actual newlines for code snippets) instead of as a sexp. *)
val test_raise : string -> unit
