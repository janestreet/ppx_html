open! Core
open Test_utils

let%expect_test "We do not incorrectly count escaped '#''s as module identifiers." =
  test {|<div>%{text "#hi!"}</div>|};
  [%expect {| Html_syntax.Node.div [(text "#hi!" : Virtual_dom.Vdom.Node.t)] |}]
;;

let%expect_test "Two kinds of '#'s" =
  test {|<div>%{text "#hi!"#Module}</div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [(Html_syntax.Node.text (Module.to_string (text "#hi!")) : Virtual_dom.Vdom.Node.t)]
    |}]
;;

let%expect_test "Normal case" =
  test {|<div>%{text "#hi!"#Module}</div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [(Html_syntax.Node.text (Module.to_string (text "#hi!")) : Virtual_dom.Vdom.Node.t)]
    |}]
;;

let%expect_test "Object case return" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<div>%{foo#bar "#hi"}</div>|});
  [%expect {| ("Expected a module identifier (e.g. Foo)") |}]
;;

let%expect_test "Object case access" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<div>%{foo#bar}</div>|});
  [%expect {| ("Expected a module identifier (e.g. Foo)") |}]
;;

let%test_module "Other contexts" =
  (module struct
    let%expect_test "Tag EXPR" =
      test {|<%{f "#hi!"}></>|};
      [%expect {| f "#hi!" [] |}]
    ;;

    let%expect_test "ATTR" =
      test {|<div %{"#hi"}></div>|};
      [%expect {| Html_syntax.Node.div ~attrs:[("#hi" : Virtual_dom.Vdom.Attr.t)] [] |}];
      test {|<div %{"#hi"#Foo}></div>|};
      [%expect
        {|
        Html_syntax.Node.div ~attrs:[(Foo.to_attr "#hi" : Virtual_dom.Vdom.Attr.t)]
          []
        |}]
    ;;

    let%expect_test "ATTR VALUE" =
      test {|<div foo=%{"#hi"}></div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.foo "#hi" : Virtual_dom.Vdom.Attr.t)] []
        |}];
      test {|<div foo=%{"#hi"#Foo}></div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.foo (Foo.to_string "#hi") : Virtual_dom.Vdom.Attr.t)]
          []
        |}]
    ;;

    let%expect_test "Option interpolation" =
      test {|<div ?{"#hi"} ?{"#hi"#Foo}>?{"#hi"} ?{"#hi"#Foo}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[((match "#hi" with | None -> Html_syntax.Attr.empty | Some x -> x) :
                 Virtual_dom.Vdom.Attr.t);
                 ((match "#hi" with
                   | None -> Html_syntax.Attr.empty
                   | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)]
          [((match "#hi" with | None -> Html_syntax.Node.none | Some x -> x) :
          Virtual_dom.Vdom.Node.t);
          Html_syntax.Node.text " ";
          ((match "#hi" with
            | None -> Html_syntax.Node.none
            | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)]
        |}]
    ;;

    let%expect_test "List interpolation" =
      test {|<div *{"#hi"} *{"#hi"#Foo}>*{"#hi"} *{"#hi"#Foo}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.many "#hi" : Virtual_dom.Vdom.Attr.t);
                 (Html_syntax.Attr.many
                    (Ppx_html_runtime.List.map "#hi" ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
          [(Html_syntax.Node.fragment "#hi" : Virtual_dom.Vdom.Node.t);
          Html_syntax.Node.text " ";
          (Html_syntax.Node.fragment
             (Ppx_html_runtime.List.map "#hi"
                ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x))) : Virtual_dom.Vdom.Node.t)]
        |}]
    ;;
  end)
;;
