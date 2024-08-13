open! Core
open Test_utils

let%expect_test "Comments are supported" =
  test
    {|<div>
        Capybaras are the world's largest living rodent.
        <!-- For now. -->
      </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      [Html_syntax.Node.text " Capybaras are the world's largest living rodent. "]
    |}]
;;

let%expect_test "Commments in between things." =
  test
    {|
    <div>
        Capybaras are the world's largest living rodent.
        <!-- I am in-between... -->
        Capybaras are the world's largest living rodent.
      </div>
  |};
  [%expect
    {|
    Html_syntax.Node.div
      [Html_syntax.Node.text " Capybaras are the world's largest living rodent. ";
      Html_syntax.Node.text " Capybaras are the world's largest living rodent. "]
    |}]
;;
