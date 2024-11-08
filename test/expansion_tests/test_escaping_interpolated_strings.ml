open! Core
open Test_utils

let%expect_test "Misparsing closing parens" =
  (* NOTE: This is a regression test against a situation where the "}" ended up closing
     the intepolated Ocaml early... *)
  test {|<div>%{Vdom.Node.text "}"}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div [(Vdom.Node.text "}" : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,1 +1,1
    -|Html_syntax.Node.div [(Vdom.Node.text "}" : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [Vdom.Node.text "}"]
    |}]
;;

let%expect_test "Nested ppx_html inside of itself." =
  test {|<div>%{[%html {xx|<div></div>|xx}]}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [([%html {xx|<div></div>|xx}] : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,2 +1,1
    -|Html_syntax.Node.div
    -|  [([%html {xx|<div></div>|xx}] : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [[%html {xx|<div></div>|xx}]]
    |}]
;;
