open! Core
open Test_utils

let%expect_test "Testing with non-default syntax" =
  test ~html_syntax_module:"Capybara" {| <munch carrot="hi">%{EXPR}</munch> |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Capybara.Html_syntax.Html_syntax.Node.munch
      ~attrs:[(Capybara.Html_syntax.Html_syntax.Attr.carrot "hi" : Virtual_dom.Vdom.Attr.t)]
      [(EXPR : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,3 +1,2
      Capybara.Html_syntax.Html_syntax.Node.munch
    -|  ~attrs:[(Capybara.Html_syntax.Html_syntax.Attr.carrot "hi" : Virtual_dom.Vdom.Attr.t)]
    -|  [(EXPR : Virtual_dom.Vdom.Node.t)]
    +|  ~attrs:[Capybara.Html_syntax.Html_syntax.Attr.carrot "hi"] [EXPR]
    |}]
;;
