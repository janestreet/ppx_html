open! Core
open Test_utils

let%expect_test "No whitespace should be allowed after a tag" =
  (* NOTE: This is a regression against html and prettier's behavior. In this instance
     prettier crashes, and html considers it as if it were a string... I think failing
     here is the correct behavior. *)
  test_raise {|< div></div>|};
  [%expect
    {|
    Expected a valid HTML tag, but instead found whitespace. No whitespace is allowed here..
      |
    0 | < div></div>
      |  ^
      |
    |}]
;;
