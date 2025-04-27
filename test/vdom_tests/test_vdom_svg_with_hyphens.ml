open! Core
open Test_util

let%expect_test "This test case demonstrates a PPX HTML example using [virtual_dom_svg]." =
  test
    (* $MDX part-begin=inline-example *)
    [%html.Virtual_dom_svg
      {|
        <svg height=%{100.} width=%{100.}>
          <circle
            cx=%{50.}
            cy=%{50.}
            r=%{40.}
            stroke=%{`Name "black"}
            stroke_width=%{3.}
            fill=%{`Name "red"}
          ></circle>
        </svg>
      |}]
  (* $MDX part-end *);
  [%expect
    {|
    <svg height="100" width="100">
      <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red"> </circle>
    </svg>
    |}]
;;

let%expect_test "This test case demonstrates a PPX HTML example using [virtual_dom_svg]." =
  (test
   @@
   (* $MDX part-begin=open-example *)
   let open Virtual_dom_svg.Html_syntax in
   {%html|
     <svg height=%{100.} width=%{100.}>
       <circle
         cx=%{50.}
         cy=%{50.}
         r=%{40.}
         stroke=%{`Name "black"}
         stroke_width=%{3.}
         fill=%{`Name "red"}
       ></circle>
     </svg>
   |}
   (* $MDX part-end *));
  [%expect
    {|
    <svg height="100" width="100">
      <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red"> </circle>
    </svg>
    |}]
;;
