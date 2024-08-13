open! Core
open Test_utils

let%expect_test "Module expansions - attribute value" =
  test
    {|<div width=%{1#Int}>
  </div>|};
  [%expect
    {|
    Html_syntax.Node.div
      ~attrs:[(Html_syntax.Attr.width (Int.to_string 1) : Virtual_dom.Vdom.Attr.t)]
      []
    |}]
;;

let%expect_test "Module expansions - node" =
  test
    {|<div>
    %{x#Foo}
  </div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [(Html_syntax.Node.text (Foo.to_string x) : Virtual_dom.Vdom.Node.t)]
    |}]
;;

let%expect_test "Module expansions - attribute" =
  test
    {|<div %{attr#Foo}>
  </div>|};
  [%expect
    {| Html_syntax.Node.div ~attrs:[(Foo.to_attr attr : Virtual_dom.Vdom.Attr.t)] [] |}]
;;
