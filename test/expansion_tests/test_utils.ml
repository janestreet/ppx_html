open! Core
open Ppxlib
open Ppx_html_syntax

let get_model_as_string ?html_syntax_module s =
  let open Ppx_html_expander in
  let loc = Location.none in
  let html_syntax_module =
    Option.map html_syntax_module ~f:(fun s -> { txt = Lident s; loc })
  in
  let parsed runtime_kind =
    s
    |> Model_parser.of_string ~loc
    |> Model_code_gen.code ~loc ~html_syntax_module ~runtime_kind
  in
  let js_of_ocaml = Pprintast.string_of_expression (parsed Js_of_ocaml) in
  let kernel = Pprintast.string_of_expression (parsed Kernel) in
  match String.equal js_of_ocaml kernel with
  | true -> Or_error.return js_of_ocaml
  | false ->
    Or_error.error_string
      ("Difference between ppx_html and ppx_html_kernel\n\n"
       ^ "PPX_HTML:\n"
       ^ js_of_ocaml
       ^ "\n\nPPX_HTML_KERNEL (diff):\n"
       ^ Expect_test_patdiff.patdiff js_of_ocaml kernel)
;;

(* We're doing a string compare here. We normalize the strings so they should be
   equivalent if the ASTs are equivalent *)
let are_models_equivalent a b =
  let tester =
    let%bind.Or_error a = get_model_as_string a in
    let%bind.Or_error b = get_model_as_string b in
    match String.equal a b with
    | true -> Or_error.return "Equivalent"
    | false ->
      Or_error.error_string
        {%string|
Models are not equivalent!

a:
%{a}

b:
%{b}
|}
  in
  match tester with
  | Ok ok -> print_endline ok
  | Error error -> Error.to_string_hum error |> print_endline
;;

let test ?html_syntax_module s =
  match get_model_as_string ?html_syntax_module s with
  | Ok js_of_ocaml ->
    print_endline "same output between ppx_html and ppx_html_kernel\n";
    print_endline js_of_ocaml
  | Error error -> Error.to_string_hum error |> print_endline
;;

let test_raise s =
  match test s with
  | () -> print_endline "Expected an error but test succeeded."
  | exception exn ->
    (match Ppxlib.Location.Error.of_exn exn with
     | Some error -> print_endline (Ppxlib.Location.Error.message error)
     | None -> print_s [%sexp (exn : exn)])
;;
