open! Core
open! Ppx_html_syntax
open! Ppxlib

val code
  :  loc:Location.t
  -> html_syntax_module:longident loc option
  -> runtime_kind:Runtime_kind.t
  -> string Ppxlib.Loc.t
  -> Model.Attr.Value.t
  -> expression

val value_to_expression
  :  html_syntax_module:longident loc option
  -> runtime_kind:Runtime_kind.t
  -> Model.Attr.Value.t
  -> expression

val argument
  :  name:label loc
  -> argument:Model.Expr.t option
  -> sigil:Model.Attr.Sigil.t
  -> runtime_kind:Runtime_kind.t
  -> html_syntax_module:longident loc option
  -> arg_label * expression
