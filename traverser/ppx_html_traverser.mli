open! Core
open Ppx_html_syntax.Model

(** [iter] also iterates all [Ppxlib.Ast.expression]s and parses any [ppx_html] statements
    found within them, then runs the iterator on said expressions. This is different
    behavior from [Ppx_html_syntax.Model.Traverse.iter], which does not parse nested
    [ppx_html] expressions *)
class iter : object
  method attr : Attr.t -> unit
  method attr_value : Attr.Value.t -> unit
  method sigil : Attr.Sigil.t -> unit
  method expr : Expr.t -> unit
  method element : Element.t -> unit
  method argument : Attr.Argument.t -> unit
  method list : ('a -> unit) -> 'a list -> unit
  method location : Location.t -> unit
  method node : Node.t -> unit
  method ocaml_expr : Ocaml_expr.t -> unit
  method escape_kind : Escape_kind.t -> unit
  method interpolation_kind : Interpolation_kind.t -> unit
  method literal : Literal.t -> unit
  method longident : Ppxlib.Longident.t -> unit
  method option : ('a -> unit) -> 'a option -> unit
  method quote : Quote.t -> unit
  method quote_elt : Quote.Elt.t -> unit
  method string : string -> unit
  method int : int -> unit
  method bool : bool -> unit
  method closing_tag : Closing_tag.t -> unit
  method string_relative_location : String_relative_location.t -> unit
  method tag : Tag.t -> unit
  method with_loc : ('a -> unit) -> 'a Ppxlib.Loc.t -> unit
end
