open! Core

val are_models_equivalent
  :  ?skip_whitespace_behavior_check:bool
  -> string
  -> string
  -> unit

val test
  :  ?skip_whitespace_behavior_check:bool
  -> ?html_syntax_module:string
  -> string
  -> unit
