open! Core
open Ppx_html_syntax
open! Model
open! Ppxlib
module C = Ast_builder.Default

let value_to_expression ~html_syntax_module value =
  match value with
  | Model.Attr.Value.Expr e -> Expr_code_gen.expr ~html_syntax_module e
  | Literal str -> Expr_code_gen.quote ~html_syntax_module str
;;

let generic_code ~use_create ~loc ~html_syntax_module name (value : Model.Attr.Value.t)
  : expression
  =
  let name_expr =
    match use_create with
    | true -> Shared.attr_fn_with_create ~loc:name.loc ~html_syntax_module name.txt
    | false -> Shared.attr_fn ~loc:name.loc ~html_syntax_module name.txt
  in
  let value_expression = value_to_expression ~html_syntax_module value in
  [%expr [%e name_expr] [%e value_expression]]
;;

let class_code ~loc ~html_syntax_module = function
  | Model.Attr.Value.Expr e ->
    [%expr
      [%e Shared.attr_fn ~loc ~html_syntax_module "class_"]
        [%e Expr_code_gen.expr ~html_syntax_module e]]
  | Literal classes ->
    let e =
      Model.Quote.split_on_space classes
      |> List.map ~f:(fun class_ -> Expr_code_gen.quote ~html_syntax_module class_)
      |> Ppxlib.Ast_builder.Default.elist ~loc
    in
    [%expr [%e Shared.attr_fn ~loc ~html_syntax_module "classes"] [%e e]]
;;

let style_code ~loc ~html_syntax_module = function
  | Model.Attr.Value.Expr e ->
    [%expr
      [%e Shared.attr_fn ~loc ~html_syntax_module "style"]
        [%e Expr_code_gen.expr ~html_syntax_module e]]
  | Literal css ->
    Expr_code_gen.with_immediate_quote css ~f:(fun css ->
      let css_string_expression =
        C.pexp_constant
          ~loc:css.loc
          (Pconst_string (Model.Quote.to_source css, css.loc, None))
      in
      [%expr [%css [%e css_string_expression]]])
;;

let tailwind_code ~loc = function
  | Model.Attr.Value.Expr e ->
    Location.raise_errorf
      ~loc:e.loc
      "Error: Tailwind support for dynamic classes is not supported. Tailwind only \
       supports string literals."
  | Literal css ->
    Expr_code_gen.with_immediate_quote css ~f:(fun css ->
      let css_string_expression =
        C.pexp_constant
          ~loc:css.loc
          (Pconst_string (Model.Quote.to_source css, css.loc, None))
      in
      [%expr [%tailwind [%e css_string_expression]]])
;;

let code ~loc ~html_syntax_module name (value : Model.Attr.Value.t) =
  match name.txt with
  | "class" -> class_code ~html_syntax_module ~loc value
  | "style" -> style_code ~html_syntax_module ~loc value
  | "tailwind" -> tailwind_code ~loc value
  | txt when String.is_prefix ~prefix:"data-" txt ->
    generic_code ~use_create:true ~loc ~html_syntax_module name value
  | txt when String.mem txt '-' ->
    Location.raise_errorf
      ~loc:name.loc
      "ppx_html attribute keys cannot have hyphens (with the exception of data-* \
       attributes)"
  | txt when Ppxlib.Keyword.is_keyword txt ->
    generic_code
      ~use_create:false
      ~loc
      ~html_syntax_module
      { name with txt = txt ^ "_" }
      value
  | _ -> generic_code ~use_create:false ~loc ~html_syntax_module name value
;;
