open! Core
open Test_utils

let%expect_test "data-test attribute" =
  test {|<div data-test=foo></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-test") "foo" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    -1,3 +1,1
    -|Html_syntax.Node.div
    -|  ~attrs:[((Html_syntax.Attr.create "data-test") "foo" : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|Html_syntax.Node.div ~attrs:[(Html_syntax.Attr.create "data-test") "foo"] []
    |}]
;;

let%expect_test "data-test attribute with interpolation" =
  test {|<div data-test=%{foo}></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-test") foo : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    -1,3 +1,1
    -|Html_syntax.Node.div
    -|  ~attrs:[((Html_syntax.Attr.create "data-test") foo : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|Html_syntax.Node.div ~attrs:[(Html_syntax.Attr.create "data-test") foo] []
    |}];
  test {|<div data-test="hi__%{foo}"></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-test") ([%string "hi__%{(foo)}"]) :
             Virtual_dom.Vdom.Attr.t)] []

    PPX_HTML_KERNEL (diff):
    -1,3 +1,3
      Html_syntax.Node.div
    -|  ~attrs:[((Html_syntax.Attr.create "data-test") ([%string "hi__%{(foo)}"]) :
    -|         Virtual_dom.Vdom.Attr.t)] []
    +|  ~attrs:[(Html_syntax.Attr.create "data-test") ([%string "hi__%{(foo)}"])]
    +|  []
    |}]
;;

let%expect_test "other kinds of data-* attributes" =
  test {|<div data-columns=foo></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-columns") "foo" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    -1,3 +1,2
    -|Html_syntax.Node.div
    -|  ~attrs:[((Html_syntax.Attr.create "data-columns") "foo" : Virtual_dom.Vdom.Attr.t)]
    +|Html_syntax.Node.div ~attrs:[(Html_syntax.Attr.create "data-columns") "foo"]
        []
    |}];
  test {|<div data-rows=foo></div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-rows") "foo" : Virtual_dom.Vdom.Attr.t)]
      []

    PPX_HTML_KERNEL (diff):
    -1,3 +1,1
    -|Html_syntax.Node.div
    -|  ~attrs:[((Html_syntax.Attr.create "data-rows") "foo" : Virtual_dom.Vdom.Attr.t)]
    -|  []
    +|Html_syntax.Node.div ~attrs:[(Html_syntax.Attr.create "data-rows") "foo"] []
    |}]
;;

let%expect_test "other kinds of foo-* attributes" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div foo-columns=foo></div>|});
  [%expect
    {| ("ppx_html attribute keys cannot have hyphens (with the exception of data-* attributes)") |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div foo-test=foo></div>|});
  [%expect
    {| ("ppx_html attribute keys cannot have hyphens (with the exception of data-* attributes)") |}]
;;
