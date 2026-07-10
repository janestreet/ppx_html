open! Core
open Test_utils

let%expect_test "Error message that we show when someone forgets to wrap nested \
                 arguments with parens"
  =
  test_raise {|<Foo.f ~child1:<div></div>></>|};
  [%expect
    {|
    ppx_html expressions must be wrapped in parentheses when used as arguments
      |
    0 | <Foo.f ~child1:<div></div>></>
      |                ^ invalid argument here
      |
    |}]
;;
