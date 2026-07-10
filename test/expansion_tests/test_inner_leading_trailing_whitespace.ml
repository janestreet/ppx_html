open! Core
open Test_utils

let%expect_test "Content inside of html tag should have a leading space if the entire \
                 opening tag is on the same line as the start of the content "
  =
  test {|<div      > Capybara's are the world's largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         " Capybara's are the world's largest living rodent."]
    |}];
  test {|<div>      Capybara's are the world's largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "      Capybara's are the world's largest living rodent."]
    |}]
;;

let%expect_test "White space in text content between non-whitespace blocks should be \
                 collapsed into a single space (including newlines)"
  =
  (* Proof here:
     https://typescriptlang.org/play/?ssl=4&ssc=1&pln=4&pc=54#code/DwEwlgbgfAwghgBwJ4CM4Cc4GcAEe8YCmAUKXgC4AWh+eA7gPboA2IAZAI4CuD5A3LmYYA5oSzkczSGAB2wnOgYhCM8gDo8wAPThoQA
  *)
  test
    {|<div>Capybaras     are


  the     world&quot;s largest living rodent.  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras     are the     world&quot;s largest living rodent.  "]
    |}]
;;

let%expect_test "Newlines before the non-space content should cause leading whitespace \
                 to be trimmed"
  =
  test
    {|<div> 
          Capybara's are the world's largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybara's are the world's largest living rodent."]
    |}]
;;

let%expect_test "Newlines inside of the opening tag should not cause trimming" =
  let ( (* https://typescriptlang.org/play/?ssl=3&ssc=1&pln=3&pc=15#code/FAHgJglgbsB8AEALApgG1Qe3iA9JKsQA
        *) )
    =
    test
      {|<div

  > Capybara's are the world's largest living rodent.</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.Primitives.text
           " Capybara's are the world's largest living rodent."]
      |}]
  in
  test
    {|<div      




    >                         Capybara's are the world's largest living rodent.</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "                         Capybara's are the world's largest living rodent."]
    |}];
  let ( (* [jsx] allows for this to be parsed and it _does_ trim the leading whitespace.
           Adding this as a regression test here because at the time of writing,
           [ppx_html] does not parse this.

           https://typescriptlang.org/play/?ssl=3&ssc=1&pln=4&pc=1#code/FAHmBMEsDdgPgAQAsCmAbNB7BID0Vo4g
        *) )
    =
    test_raise
      {|<
    div>                         Capybara's are the world's largest living rodent.</div>|};
    [%expect
      {|
      Expected a valid HTML tag, but instead found whitespace. No whitespace is allowed here..
        |
      0 | <
        |  ^
      1 |     div>                         Capybara's are the world's largest living rodent.</div>
        |
      |}]
  in
  ()
;;

let%expect_test "Content should have trailing whitespace if there is no newline within \
                 the trailing whitespace of the text content"
  =
  test {|<div>Capybaras are the world&quot;s largest living rodent.  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent.  "]
    |}];
  test
    {|<div>Capybaras are the world&quot;s largest living rodent.  </
  div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent.  "]
    |}];
  test
    {|<div>Capybaras are the world&quot;s 



    largest 



       living rodent.  </
  div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent.  "]
    |}]
;;

let%expect_test "Newlines within the closing tag do not affect the trailing whitespace" =
  (* This one is different from how the opening whitespace behaves and I'm not entirely
     sure why they made it like this but them's the breaks I guess

     https://playground.react.dev/#N4Igzg9grgTgxgUxALhAgHgBwjALgAgBMEAzAQygBsCSoA7OXASwjvwFkBPAQU0wAoAlPmAAdNvhgJcsNv3H5F+ADxhMZOgD58ZMPgBGSkioD0CpWo2bzg8QF9xIADQg4rEkwDmKEEwC22Hj4uJyYCCL4AAqUUJ5MdADymMysenb4JDAQfvgA5Ppk+giUALSYMXF0JVJkjCVuAUyUCDAmhExguLkA3OLi-GISJiYNmE1kKXTsEMTI+KIgZJSUC-b4YBMdHgh60bHxSZNggt3O4AAWEADuAJJ0uC10S2Ao5JRgCHZAA
  *)
  test
    {|<div>Capybara's are the world's largest living rodent.      </
    div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybara's are the world's largest living rodent.      "]
    |}]
;;

let%expect_test "Newlines within inner content do not trim whitespace" =
  (* https://typescriptlang.org/play/?ssl=1&ssc=1&pln=2&pc=1#code/FAHgJglgbgfABACQKYBsUHs7mjYEB2+SATlgPSSxwDq6xKY5lMQA
  *)
  test
    {|<div> Hello <div>
  inner  </div> World </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text " Hello ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "inner  "];
      Html_syntax.Node.Primitives.text " World "]
    |}]
;;

let%expect_test "Content should NOT have trailing whitespace if the closing tag is on a \
                 different line"
  =
  test
    {|<div>Capybaras are the world&quot;s largest living rodent.  


  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent."]
    |}];
  test
    {|<div>Capybaras are the world&quot;s largest living 

    rodent.  


  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent."]
    |}]
;;

let%expect_test "Whitespace inside text content should be collapsed into singular space" =
  test
    {|<div>Capybaras are the world&quot;s largest living 






                               rodent.  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living rodent.  "]
    |}];
  test
    {|<div>Capybaras are the world&quot;s largest living                             rodent.  </div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text
         "Capybaras are the world&quot;s largest living                             rodent.  "]
    |}]
;;

let%expect_test "Spaces are preserved between tags that show up on the same line" =
  (* https://typescriptlang.org/play/?#code/DwEwlgbgfABDwGcAOBDAdlFwD0z2zlEigCMdxpDsKog
  *)
  test
    {|
<div>   <span>a</span>   <div>b</div>    </div> 
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text "   ";
      Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.Primitives.text "   ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "b"];
      Html_syntax.Node.Primitives.text "    "]
    |}];
  test
    {|
    <div>
      <span>a</span>   <div>b</div>
    </div> 
    |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.Primitives.text "   ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "b"]]
    |}];
  (* https://typescriptlang.org/play/?#code/DwEwlgbgfABDBQc7AM4AcCGA7KHgHp1tZlwJEkoAjAsk+WyKIA
  *)
  test
    {|
    <div>
      <span>a</span>   <div
      >b</div>
    </div> 
    |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.Primitives.text "   ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "b"]]
    |}];
  (* https://typescriptlang.org/play/?ssl=2&ssc=1&pln=3&pc=1#code/DwEwlgbgfAUABHYBnADgQwHZTcA9POVTKBRcCAqAIz3NlsliA
  *)
  test
    {|
    <div>
      <span>a</
      span>   <div
      >b</div>
    </div> 
    |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.Primitives.text "   ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "b"]]
    |}];
  (* https://typescriptlang.org/play/?#code/DwEwlgbgfAUABHYBnADgQwHbzlNwD02qmUCi4E2UARgRbHZLEA
  *)
  test
    {|
    <div>
      <span
      >a</
      span>   <div
      >b</div>
    </div> 
    |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.Primitives.text "   ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "b"]]
    |}];
  (* This should all show up on a single line with spaces between each letter

     https://typescriptlang.org/play/?#code/AQ4HgEwSwNwPgFClGAzgBwIYDs6bAPQY5zjTwBGCCAxksoeaZLHAKaOv0ifwJA
  *)
  test
    {|
        <div>
          <span>a</span> <div>b

    c
         </div> <div>e</div>
        </div>
    |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.Primitives.text " ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "b c"];
      Html_syntax.Node.Primitives.text " ";
      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "e"]]
    |}]
;;

let%expect_test "No spaces are added if there are new lines between the two adjacent \
                 html nodes"
  =
  test
    ({|
    <div>
    <span>a</span>                     |}
     ^ {|


                         <span>b</span>

    </div>

    |}
    );
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [Html_syntax.Node.span [Html_syntax.Node.Primitives.text "a"];
      Html_syntax.Node.span [Html_syntax.Node.Primitives.text "b"]]
    |}]
;;

let%expect_test "Explicit whitespaces are preserved" =
  test {|<div>#{" "}sandwiched#{"\n"}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div
      [(Html_syntax.Node.Primitives.text ((" ")[@merlin.focus ]) : _);
      Html_syntax.Node.Primitives.text "sandwiched";
      (Html_syntax.Node.Primitives.text (("\n")[@merlin.focus ]) : _)]
    |}]
;;

let%expect_test "Element with no whitespace surrounding inner content should be \
                 equivalent to element with inner content on a new line"
  =
  are_models_equivalent
    {|<div>test</div>|}
    {|
  <div>
    test
  </div>
  |};
  [%expect {| Equivalent |}]
;;

let%expect_test "Consecutive content nodes reset the leading/trailing space trimming \
                 paradigm"
  =
  (* What this means is that if you have [Text] [Expr] [Text], then the trailing [Text]
     will have a space so long as there is no newline within it

     https://typescriptlang.org/play/?#code/DwPgUAhgBDsw3gIjogvrARmUYbTrEiqrlFsAPTjAAmAlgG7h4GHKxqbYX1Pa-Mo+VkQ4kY5Ho2pUwQA
  *)
  test
    {|

    <>
    a        #{"    "}   b
    <>
      a        #{"    "}
      b
    </>
    <div>
      a        #{"    "}   b
    </div>
    <div>
      a        #{"    "}
      b
    </div>
    </>
    |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.fragment
      [Html_syntax.Node.Primitives.text "a        ";
      (Html_syntax.Node.Primitives.text (("    ")[@merlin.focus ]) : _);
      Html_syntax.Node.Primitives.text "   b";
      Html_syntax.Node.Primitives.fragment
        [Html_syntax.Node.Primitives.text "a        ";
        (Html_syntax.Node.Primitives.text (("    ")[@merlin.focus ]) : _);
        Html_syntax.Node.Primitives.text "b"];
      Html_syntax.Node.div
        [Html_syntax.Node.Primitives.text "a        ";
        (Html_syntax.Node.Primitives.text (("    ")[@merlin.focus ]) : _);
        Html_syntax.Node.Primitives.text "   b"];
      Html_syntax.Node.div
        [Html_syntax.Node.Primitives.text "a        ";
        (Html_syntax.Node.Primitives.text (("    ")[@merlin.focus ]) : _);
        Html_syntax.Node.Primitives.text "b"]]
    |}]
;;
