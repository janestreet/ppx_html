open! Core
open Test_utils

let%expect_test "Extra top-level unopened HTML tags." =
  test_raise {|<div></div></div>|};
  [%expect
    {|
    This closing tag was never opened.
      |
    0 | <div></div></div>
      |            ^ closing tag here
      |
    |}]
;;
