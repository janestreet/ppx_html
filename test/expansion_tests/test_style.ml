open! Core
open Test_utils

let%expect_test "Style" =
  test
    {|
    <div style="background-color: tomato"> </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: tomato"] : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.text " "]
    |}]
;;

let%expect_test "Many styles" =
  test
    {|
    <div style="background-color: tomato; background-color: red"> </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: tomato; background-color: red"] :
             Virtual_dom.Vdom.Attr.t)] [Html_syntax.Node.text " "]
    |}]
;;

let%expect_test "Interpolation within styles" =
  (* NOTE: We want to make ppx_css do this. *)
  test
    {|
    <div style="background-color: tomato; background-color: %{color}"> </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: tomato; background-color: %{(color)}"] :
             Virtual_dom.Vdom.Attr.t)] [Html_syntax.Node.text " "]
    |}]
;;

let%expect_test "Whole-sale interpolation does not call ppx_css" =
  (* NOTE: We do not want to make ppx_css do this. *)
  test
    {|
    <div style=%{Css_gen.foo}> </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.style Css_gen.foo : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.text " "]
    |}]
;;

let%expect_test "PPX CSS's interpolation syntax" =
  test
    {|
    <div style="background-color: %{color#Color}"> </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%css "background-color: %{(color)#Color}"] : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.text " "]
    |}]
;;
