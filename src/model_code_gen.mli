open! Core
open! Ppxlib
open Ppx_html_syntax

val code
  :  loc:Location.t
  -> html_syntax_module:Ppxlib.Longident.t loc option
  -> Model.Node.t list
  -> expression
