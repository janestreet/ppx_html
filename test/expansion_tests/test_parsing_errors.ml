open! Core
open Test_utils

let%expect_test "Misparse - due to mismatch in tags" =
  test_raise {| <h1>cool</h2> |};
  [%expect
    {|
    Expected closing tag </h1>, but got </h2>.
      |
    0 |  <h1>cool</h2>
      |          ^^^^^ invalid closing tag here
      |  ^^^^ opening tag found here
      |
    |}]
;;

let%expect_test "Misparse - due to mismatch in tags" =
  test_raise {| <h1>cool</> |};
  [%expect
    {|
    Expected </h1>, but got an empty closing tag (</>).
      |
    0 |  <h1>cool</>
      |          ^^^ invalid closing tag here
      |  ^^^^ opening tag found here
      |
    |}]
;;

let%expect_test "No closing tag." =
  test_raise {| <div> |};
  [%expect
    {|
    Expected closing tag for 'div', found EOF instead
      |
    0 |  <div>
      |  ^^^^^ opening tag found here
      |
    |}]
;;

let%expect_test "Misparsed OCaml expression" =
  test_raise
    {|

  <div>

       %{

       let x = = = = = = = 1 in
       ()

       }


  </div>


  |};
  [%expect
    {|
    Failed to parse OCaml expression inside of HTML.
    File "_none_", line 6, characters 15-16:
                                                     Error: Syntax error
    |}]
;;
