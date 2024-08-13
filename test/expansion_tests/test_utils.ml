open! Core
open Ppxlib
open Ppx_html_syntax

let test ?html_syntax_module s =
  let open Ppx_html in
  let loc = Location.none in
  let html_syntax_module =
    Option.map html_syntax_module ~f:(fun s -> { txt = Lident s; loc })
  in
  s
  |> Model_parser.of_string ~loc
  |> Model_code_gen.code ~loc ~html_syntax_module
  |> Pprintast.string_of_expression
  |> print_endline
;;
