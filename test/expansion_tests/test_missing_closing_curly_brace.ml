open! Core
open Test_utils

let%expect_test "Test nice error message on missing curly brace" =
  test_raise
    {|
    <div>

    %{ Vdom.Node.text "hi"

  |};
  [%expect
    {|
    Missing curly brace for interpolated OCaml
      |
    2 |
    3 |     %{ Vdom.Node.text "hi"
      |                           ^ curly brace expected here
    4 |
      |
    |}]
;;

let%expect_test "Test nice error message on missing brace with nesting" =
  test_raise
    {x|
    <div>

    %{ Vdom.Node.text [%string {| %{"hi"} |}]</div>

  |x};
  [%expect
    {xxx|
    Missing curly brace for interpolated OCaml
      |
    2 |
    3 |     %{ Vdom.Node.text [%string {| %{"hi"} |}]</div>
      |                                                    ^ curly brace expected here
    4 |
      |
    |xxx}]
;;
