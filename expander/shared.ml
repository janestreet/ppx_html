open! Core
open Ppxlib
open Ast_builder.Default

let html_syntax = "Html_syntax"

let html_syntax ~html_syntax_module : Longident.t =
  match html_syntax_module with
  | None -> Lident html_syntax
  | Some id -> Ldot (Ldot (id.txt, html_syntax), html_syntax)
;;

let html_syntax_fn ~loc ~html_syntax_module ~submodule fn =
  let lid : Longident.t = Ldot (Ldot (html_syntax ~html_syntax_module, submodule), fn) in
  pexp_ident ~loc (Located.mk ~loc lid)
;;

let node_fn ~loc ~html_syntax_module fn =
  html_syntax_fn ~loc ~html_syntax_module ~submodule:"Node" fn
;;

let attr_fn ~loc ~html_syntax_module fn =
  html_syntax_fn ~loc ~html_syntax_module ~submodule:"Attr" fn
;;

let attr_fn_with_create ~loc ~html_syntax_module fn =
  let create = html_syntax_fn ~loc ~html_syntax_module ~submodule:"Attr" "create" in
  let name = Ast_helper.Exp.constant ~loc (Ast_helper.Const.string ~loc fn) in
  [%expr [%e create] [%e name]]
;;

let virtual_dom_t_type ~loc ~submodule =
  let lid : Longident.t loc =
    { txt = Ldot (Ldot (Ldot (Lident "Virtual_dom", "Vdom"), submodule), "t"); loc }
  in
  ptyp_constr ~loc lid []
;;

let attr_t_type ~loc = virtual_dom_t_type ~loc ~submodule:"Attr"
let node_t_type ~loc = virtual_dom_t_type ~loc ~submodule:"Node"
