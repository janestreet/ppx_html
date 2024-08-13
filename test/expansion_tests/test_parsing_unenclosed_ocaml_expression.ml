open! Core
open Test_utils

let%expect_test "Parsing unparenthesized ocaml expression" =
  test {|<div>%{x : Vdom.Node.t}</div>|};
  [%expect {| Html_syntax.Node.div [((x : Vdom.Node.t) : Virtual_dom.Vdom.Node.t)] |}]
;;

let%expect_test "Parsing parenthesized ocaml expression" =
  test {|<div>%{(x : Vdom.Node.t)}</div>|};
  [%expect {| Html_syntax.Node.div [((x : Vdom.Node.t) : Virtual_dom.Vdom.Node.t)] |}]
;;
