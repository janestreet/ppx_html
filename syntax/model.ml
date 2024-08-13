open Core

module Interpolation_kind = struct
  type t =
    | Normal (* %{} *)
    | Option (* ?{} *)
    | List
  (* *{} *) [@@deriving sexp_of]

  let to_string = function
    | Normal -> "percent-based (%{})"
    | Option -> "option (?{})"
    | List -> "list (*{})"
  ;;
end

module Ocaml_expr = struct
  type t = Ppxlib.Ast.expression

  let to_string t =
    assert (String.is_empty (Format.flush_str_formatter ()));
    Ppxlib.Pprintast.expression Format.str_formatter t;
    Format.flush_str_formatter ()
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
end

module String_relative_location = struct
  type t =
    { start : int
    ; end_ : int
    }
  [@@deriving sexp_of]
end

module Location = struct
  include Ppxlib.Location

  let to_string { loc_start; loc_end; _ } =
    if String.( <> ) loc_start.pos_fname loc_end.pos_fname
    then
      [%string
        "%{loc_start.pos_fname}[%{loc_start.pos_lnum#Int}:%{loc_start.pos_cnum - \
         loc_start.pos_bol#Int}]-%{loc_end.pos_fname}[%{loc_end.pos_lnum#Int}:%{loc_end.pos_cnum \
         - loc_end.pos_bol#Int}]"]
    else if Int.( <> ) loc_start.pos_lnum loc_end.pos_lnum
    then
      [%string
        "%{loc_start.pos_fname}[%{loc_start.pos_lnum#Int}:%{loc_start.pos_cnum - \
         loc_start.pos_bol#Int}-%{loc_end.pos_lnum#Int}:%{loc_end.pos_cnum - \
         loc_end.pos_bol#Int}]"]
    else if Int.( <> ) loc_start.pos_cnum loc_end.pos_cnum
    then
      [%string
        "%{loc_start.pos_fname}[%{loc_start.pos_lnum#Int}:%{loc_start.pos_cnum - \
         loc_start.pos_bol#Int}-%{loc_end.pos_cnum - loc_end.pos_bol#Int}]"]
    else
      [%string
        "%{loc_start.pos_fname}[%{loc_start.pos_lnum#Int}:%{loc_start.pos_cnum - \
         loc_start.pos_bol#Int}"]
  ;;

  let sexp_of_t t = Sexp.Atom (to_string t)
  let length t = t.loc_end.pos_cnum - t.loc_start.pos_cnum
end

module Loc = struct
  type 'a t = 'a Ppxlib.Loc.t =
    { txt : 'a
    ; loc : Location.t
    }
  [@@deriving sexp_of]
end

module Expr = struct
  type t =
    { expr : Ocaml_expr.t
    ; code : string Loc.t
    ; to_t : string Loc.t option
    ; loc : Location.t
    ; string_relative_location : String_relative_location.t
    }
  [@@deriving sexp_of]

  let loc t = t.loc

  let to_source t =
    let code = t.code.txt in
    let to_t =
      match t.to_t with
      | None -> ""
      | Some to_t -> [%string "#%{to_t.txt}"]
    in
    let min_len = String.length code + String.length to_t in
    let target_len = Location.length t.loc - String.length "%{}" in
    (* Try really hard to preserve the expression length *)
    let code = String.pad_right ~char:' ' code ~len:(target_len - String.length to_t) in
    let len = String.length code + String.length to_t in
    assert (len >= min_len);
    assert (len >= target_len);
    assert (len = if target_len < min_len then min_len else target_len);
    "%{" ^ code ^ to_t ^ "}"
  ;;
end

module Quote = struct
  module Elt = struct
    type t =
      | Literal of string Loc.t
      | Expr of Expr.t
    [@@deriving sexp_of]

    let loc = function
      | Literal t -> t.loc
      | Expr t -> Expr.loc t
    ;;

    let to_source = function
      | Literal t -> t.txt
      | Expr t -> Expr.to_source t
    ;;
  end

  type t = Elt.t list Loc.t [@@deriving sexp_of]

  let loc (t : t) = t.loc

  let split_on_space ({ txt; loc = _ } : t) =
    let is_ws = function
      | Elt.Literal { txt; _ } ->
        (not (String.is_empty txt)) && String.for_all txt ~f:Char.is_whitespace
      | _ -> false
    in
    txt
    |> List.group ~break:(fun a b -> is_ws a || is_ws b)
    |> List.filter ~f:(Fn.non (List.for_all ~f:is_ws))
    |> List.map ~f:(fun txt ->
      let first = List.hd_exn txt |> Elt.loc in
      let last = List.last_exn txt |> Elt.loc in
      { Loc.txt; loc = { first with loc_end = last.loc_end } })
  ;;

  let to_source (t : t) = List.map t.txt ~f:Elt.to_source |> String.concat ~sep:""
end

module Attr = struct
  module Value = struct
    type t =
      | Literal of Quote.t
      | Expr of Expr.t
    [@@deriving sexp_of]

    let loc = function
      | Literal t -> Quote.loc t
      | Expr t -> Expr.loc t
    ;;
  end

  type t =
    | Attr of
        { name : string Loc.t
        ; value : Value.t option
        ; loc : Location.t
        }
    | Expr of
        { expr : Expr.t
        ; interpolation_kind : Interpolation_kind.t
        }
  [@@deriving sexp_of]

  let loc = function
    | Attr { name = _; value = _; loc } -> loc
    | Expr expr -> Expr.loc expr.expr
  ;;
end

module Tag = struct
  type t =
    | Literal of string Loc.t
    | Expr of Expr.t
    | Fragment of Location.t
  [@@deriving sexp_of]

  let loc = function
    | Literal t -> t.loc
    | Expr t -> Expr.loc t
    | Fragment loc -> loc
  ;;
end

module Node = struct
  type t =
    | Text of string Loc.t
    | Expr of
        { expr : Expr.t
        ; interpolation_kind : Interpolation_kind.t
        }
    | Element of
        { tag : Tag.t
        ; attrs : Attr.t list
        ; inner : t list option
        ; loc : Location.t
        ; open_loc : Location.t
        ; open_string_relative_location : String_relative_location.t
        ; close_string_relative_location : String_relative_location.t option
        }
  [@@deriving sexp_of]

  let loc = function
    | Text t -> t.loc
    | Expr t -> Expr.loc t.expr
    | Element
        { tag = _
        ; attrs = _
        ; inner = _
        ; loc
        ; open_loc = _
        ; open_string_relative_location = _
        ; close_string_relative_location = _
        } -> loc
  ;;
end

include struct
  type ocaml_expr = Ocaml_expr.t
  type location = Ppxlib.location

  type 'a with_loc = 'a Ppxlib.Loc.t =
    { txt : 'a
    ; loc : location
    }

  and interpolation_kind = Interpolation_kind.t =
    | Normal
    | Option
    | List

  and string_relative_location = String_relative_location.t =
    { start : int
    ; end_ : int
    }

  and expr = Expr.t =
    { expr : ocaml_expr
    ; code : string with_loc
    ; to_t : string with_loc option
    ; loc : location
    ; string_relative_location : string_relative_location
    }

  and quote_elt = Quote.Elt.t =
    | Literal of string with_loc
    | Expr of expr

  and quote = quote_elt list with_loc

  and attr_value = Attr.Value.t =
    | Literal of quote
    | Expr of expr

  and attr = Attr.t =
    | Attr of
        { name : string with_loc
        ; value : attr_value option
        ; loc : location
        }
    | Expr of
        { expr : expr
        ; interpolation_kind : interpolation_kind
        }

  and tag = Tag.t =
    | Literal of string with_loc
    | Expr of expr
    | Fragment of location

  and node = Node.t =
    | Text of string with_loc
    | Expr of
        { expr : expr
        ; interpolation_kind : interpolation_kind
        }
    | Element of
        { tag : tag
        ; attrs : attr list
        ; inner : node list option
        ; loc : location
        ; open_loc : location
        ; open_string_relative_location : string_relative_location
        ; close_string_relative_location : string_relative_location option
        }
  [@@deriving traverse_map]
end

module Traverse = struct
  class map' =
    object
      inherit map
      method list : 'a. ('a -> 'a) -> 'a list -> 'a list = fun f -> List.map ~f
      method option : 'a. ('a -> 'a) -> 'a option -> 'a option = fun f -> Option.map ~f
      method string : string -> string = Fn.id
      method int : int -> int = Fn.id
      method ocaml_expr : ocaml_expr -> ocaml_expr = Fn.id
      method location : location -> location = Fn.id
    end

  class map = map'
end
