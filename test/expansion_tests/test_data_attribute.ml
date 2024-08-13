open! Core
open Test_utils

let%expect_test "data-test attribute" =
  test {|<div data-test=foo></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-test") "foo" : Virtual_dom.Vdom.Attr.t)]
      []
    |}]
;;

let%expect_test "data-test attribute with interpolation" =
  test {|<div data-test=%{foo}></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-test") foo : Virtual_dom.Vdom.Attr.t)]
      []
    |}];
  test {|<div data-test="hi__%{foo}"></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-test") ([%string "hi__%{(foo)}"]) :
             Virtual_dom.Vdom.Attr.t)] []
    |}]
;;

let%expect_test "other kinds of data-* attributes" =
  test {|<div data-columns=foo></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-columns") "foo" : Virtual_dom.Vdom.Attr.t)]
      []
    |}];
  test {|<div data-rows=foo></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[((Html_syntax.Attr.create "data-rows") "foo" : Virtual_dom.Vdom.Attr.t)]
      []
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
