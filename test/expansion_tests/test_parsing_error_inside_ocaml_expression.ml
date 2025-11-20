open! Core
open Test_utils

let%expect_test "Syntax error inside of OCaml expression" =
  (* NOTE: This is a regression test against a test where the errors inside of embedded
     OCaml are eaten up. *)
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|
      <div>
        %{
         Vdom.Node.text  ( "hello"


    ^

    ^


         "world" )
         }
      </div>|});
  [%expect
    {| ("Failed to parse OCaml expression inside of HTML.\nFile \"_none_\", line 8, characters 4-5:\n                                                 Error: Syntax error\n") |}];
  test
    {|
      <div>
        %{
         Vdom.Node.text  ( "hello"


    ^


         "world" )
         }
      </div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [(Vdom.Node.text ("hello" ^ "world") : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
    -|Html_syntax.Node.div
    -|  [(Vdom.Node.text ("hello" ^ "world") : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [Vdom.Node.text ("hello" ^ "world")]
    |}]
;;
