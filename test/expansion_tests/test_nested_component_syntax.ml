open! Core
open! Test_utils

let%expect_test "Basic nested components as labeled args" =
  test {|<Foo.f ~arg1:(<div></div>)></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [] ~arg1:(Html_syntax.Node.div [])
    |}];
  test {|<Foo.f ?arg1:(<Foo.Bar.f>Hihi</Foo.Bar.f>)></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [] ?arg1:(Foo.Bar.f [Html_syntax.Node.Primitives.text "Hihi"])
    |}]
;;

let%expect_test "Nested component as optional arg" =
  test {|<Foo.f ?arg1:(<div></div>)></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [] ?arg1:(Html_syntax.Node.div [])
    |}]
;;

let%expect_test "Whitespace inside parens in nested argument" =
  test
    {|<Foo.f ~arg1:(
        <div>
          <div>Hi</div>
          <div>Hi</div>
        </div>
      ) />|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f ()
      ~arg1:(Html_syntax.Node.div
               [Html_syntax.Node.div [Html_syntax.Node.Primitives.text "Hi"];
               Html_syntax.Node.div [Html_syntax.Node.Primitives.text "Hi"]])
    |}]
;;
