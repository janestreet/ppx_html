open! Core
open! Ppx_html_syntax
open! Ppxlib

val code
  :  loc:Location.t
  -> html_syntax_module:longident loc option
  -> string Ppxlib.Loc.t
  -> Model.Attr.Value.t
  -> expression

val value_to_expression
  :  html_syntax_module:longident loc option
  -> Model.Attr.Value.t
  -> expression
