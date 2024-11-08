open! Core
open Test_utils

let%expect_test "Parsing unparenthesized ocaml expression" =
  test {|<div>%{x : Vdom.Node.t}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div [((x : Vdom.Node.t) : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,1 +1,1
    -|Html_syntax.Node.div [((x : Vdom.Node.t) : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [(x : Vdom.Node.t)]
    |}]
;;

let%expect_test "Parsing parenthesized ocaml expression" =
  test {|<div>%{(x : Vdom.Node.t)}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div [((x : Vdom.Node.t) : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,1 +1,1
    -|Html_syntax.Node.div [((x : Vdom.Node.t) : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [(x : Vdom.Node.t)]
    |}]
;;
