open! Core
open Test_utils

let%expect_test "basic expansion" =
  test {|<div tailwind="bg-white"></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t)] []
    |}]
;;

let%expect_test "multiple classes" =
  test {|<div tailwind="bg-white bg-black foo bar baz"></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white bg-black foo bar baz"] : Virtual_dom.Vdom.Attr.t)]
      []
    |}]
;;

let%expect_test "multiple tailwind attrs" =
  test {|<div tailwind="bg-white" tailwind="bg-white foo bar baz"></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t);
             ([%tailwind "bg-white foo bar baz"] : Virtual_dom.Vdom.Attr.t)] []
    |}]
;;

let%expect_test "Invalid position" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div tailwind=%{foo}></div>|});
  [%expect
    {| ("Error: Tailwind support for dynamic classes is not supported. Tailwind only supports string literals.") |}]
;;

let%expect_test "No quotes" =
  test {|<div tailwind=bg-white></div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[([%tailwind "bg-white"] : Virtual_dom.Vdom.Attr.t)] []
    |}]
;;
