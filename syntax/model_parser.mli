open! Core
open! Ppxlib

val ocaml_expression : string Ppxlib.Loc.t -> expression

(** [of_string] returns the AST representation of the provided [string].

    [?filter_empty_text_nodes] (default:true) can be used to preserve whitespace
    information in the AST. This is only really useful in treesmashes *)
val of_string
  :  ?filter_empty_text_nodes:bool
  -> loc:Location.t
  -> string
  -> Model.Node.t list

module Private : sig
  val collapse_prefix_and_trailing_ws : string -> string
  val collapse_ws : string -> string
end
