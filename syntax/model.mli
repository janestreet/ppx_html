open! Core

module Interpolation_kind : sig
  type t =
    | Normal (* %{} *)
    | Option (* ?{} *)
    | List (* *{} *)
    | String (* #{} *)
  [@@deriving sexp_of]

  val to_string : t -> string
end

module Ocaml_expr : sig
  type t = Ppxlib.Ast.expression [@@deriving sexp_of]

  val to_string : t -> string
end

module String_relative_location : sig
  type t =
    { start : int
    ; end_ : int
    }
  [@@deriving sexp_of]
end

module Expr : sig
  type t =
    { expr : Ppxlib.Ast.expression
    ; code : string Ppxlib.Loc.t
    ; to_t : string Ppxlib.Loc.t option
    ; loc : Ppxlib.Location.t
    ; string_relative_location : String_relative_location.t
    }
  [@@deriving sexp_of]

  val loc : t -> Ppxlib.Location.t
end

module Quote : sig
  module Elt : sig
    type t =
      | Literal of string Ppxlib.Loc.t
      | Expr of Expr.t
    [@@deriving sexp_of]

    val loc : t -> Ppxlib.Location.t
  end

  type t = Elt.t list Ppxlib.Loc.t [@@deriving sexp_of]

  val loc : t -> Ppxlib.Location.t
  val split_on_space : t -> t list
  val to_source : t -> string
end

module Attr : sig
  module Value : sig
    type t =
      | Literal of Quote.t
      | Expr of Expr.t
    [@@deriving sexp_of]

    val loc : t -> Ppxlib.Location.t
  end

  type t =
    | Attr of
        { name : string Ppxlib.Loc.t
        ; value : Value.t option
        ; loc : Ppxlib.Location.t
        }
    | Expr of
        { expr : Expr.t
        ; interpolation_kind : Interpolation_kind.t
        }
  [@@deriving sexp_of]

  val loc : t -> Ppxlib.Location.t
end

module Tag : sig
  type t =
    | Literal of string Ppxlib.Loc.t
    | Expr of Expr.t
    | Fragment of Location.t
  [@@deriving sexp_of]

  val loc : t -> Ppxlib.Location.t
end

module Node : sig
  type t =
    | Text of string Ppxlib.Loc.t
    | Expr of
        { expr : Expr.t
        ; interpolation_kind : Interpolation_kind.t
        }
    | Element of
        { tag : Tag.t
        ; attrs : Attr.t list
        ; inner : t list option
        ; loc : Ppxlib.Location.t
        ; open_loc : Ppxlib.Location.t
        ; open_string_relative_location : String_relative_location.t
        ; close_string_relative_location : String_relative_location.t option
        }
  [@@deriving sexp_of]

  val loc : t -> Ppxlib.Location.t
end

module Traverse : sig
  class map : object
    method attr : Attr.t -> Attr.t
    method attr_value : Attr.Value.t -> Attr.Value.t
    method expr : Expr.t -> Expr.t
    method list : ('a -> 'a) -> 'a list -> 'a list
    method location : Location.t -> Location.t
    method node : Node.t -> Node.t
    method ocaml_expr : Ocaml_expr.t -> Ocaml_expr.t
    method interpolation_kind : Interpolation_kind.t -> Interpolation_kind.t
    method option : ('a -> 'a) -> 'a option -> 'a option
    method quote : Quote.t -> Quote.t
    method quote_elt : Quote.Elt.t -> Quote.Elt.t
    method string : string -> string
    method int : int -> int

    method string_relative_location :
      String_relative_location.t -> String_relative_location.t

    method tag : Tag.t -> Tag.t
    method with_loc : ('a -> 'a) -> 'a Ppxlib.Loc.t -> 'a Ppxlib.Loc.t
  end
end
