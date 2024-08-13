open! Core
open! Ppxlib
open Ppx_html_syntax
open Model
module C = Ast_builder.Default

let sanitize_ocaml_keyword s = if Ppxlib.Keyword.is_keyword s then s ^ "_" else s

let rec node_expr : html_syntax_module:longident loc option -> Node.t -> expression =
  fun ~html_syntax_module -> function
  | Text { txt; loc } ->
    [%expr [%e Shared.node_fn ~loc ~html_syntax_module "text"] [%e C.estring ~loc txt]]
  | Expr { expr; interpolation_kind } ->
    Expr_code_gen.expr ~html_syntax_module ~type_:(Node { interpolation_kind }) expr
  | Element
      { tag
      ; attrs
      ; inner
      ; loc = full_loc
      ; open_loc = loc
      ; open_string_relative_location = _
      ; close_string_relative_location = _
      } ->
    let tag =
      match tag with
      | Literal name ->
        Shared.node_fn ~loc:name.loc ~html_syntax_module (sanitize_ocaml_keyword name.txt)
      | Expr e -> Expr_code_gen.expr ~html_syntax_module e
      | Fragment loc -> Shared.node_fn ~loc ~html_syntax_module "fragment"
    in
    let attrs, keys =
      List.partition_map attrs ~f:(function
        | Attr.Attr { name = { txt = "key"; loc = _ }; value; loc } -> Second (value, loc)
        | x -> First x)
    in
    let key_args =
      match keys with
      | [] -> []
      | _ :: (_, second_loc) :: _ ->
        Location.raise_errorf
          ~loc:second_loc
          {|Error: There can only be one key. Please remove this duplicate key.|}
      | [ (None, loc) ] ->
        Location.raise_errorf
          ~loc
          {|Error: The attribute key needs a value. (e.g. key=a-unique-key)|}
      | [ (Some value, _) ] ->
        let arg_expression =
          Attr_code_gen.value_to_expression ~html_syntax_module value
        in
        [ Labelled "key", arg_expression ]
    in
    let attrs =
      List.map attrs ~f:(function
        | Attr.Expr { expr; interpolation_kind } ->
          Expr_code_gen.expr ~html_syntax_module ~type_:(Attr { interpolation_kind }) expr
        | Attr.Attr { name; value = None; loc = _ } ->
          Shared.attr_fn
            ~loc:name.loc
            ~html_syntax_module
            (sanitize_ocaml_keyword name.txt)
        | Attr.Attr { name; value = Some value; loc } ->
          Attr_code_gen.code ~loc ~html_syntax_module name value)
    in
    let args =
      let attrs =
        if List.is_empty attrs
        then []
        else
          [ ( Labelled "attrs"
            , attrs
              |> List.map ~f:(fun e -> [%expr ([%e e] : [%t Shared.attr_t_type ~loc])])
              |> C.elist ~loc )
          ]
      in
      let nodes =
        [ ( Nolabel
          , match inner with
            | None -> [%expr ()]
            | Some inner ->
              let arg_expressions =
                inner |> List.map ~f:(fun node -> node_expr ~html_syntax_module node)
              in
              let loc =
                match arg_expressions with
                | [] -> full_loc
                | first :: _ ->
                  let last = List.last_exn arg_expressions in
                  let first, last = first.pexp_loc, last.pexp_loc in
                  { loc_start = first.loc_start
                  ; loc_end = last.loc_end
                  ; loc_ghost = false
                  }
              in
              C.elist ~loc arg_expressions )
        ]
      in
      List.concat [ key_args; attrs; nodes ]
    in
    C.pexp_apply ~loc:full_loc tag args
;;

let code ~loc ~html_syntax_module (model : Node.t list) =
  let model =
    List.filter model ~f:(function
      | Text { txt; _ } when String.for_all txt ~f:Char.is_whitespace -> false
      | _ -> true)
  in
  match model with
  | [] -> Shared.node_fn ~html_syntax_module ~loc "none"
  | [ t ] -> { (node_expr ~html_syntax_module t) with pexp_loc = loc }
  | _ :: _ as elements ->
    Location.raise_errorf
      ~loc
      "ppx_html expects to return a single html element, but found %d top-level elements."
      (List.length elements)
;;
