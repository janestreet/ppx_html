open! Core
open Test_utils

let%expect_test "We do not incorrectly count escaped '#''s as module identifiers." =
  test {|<div>%{text "#hi!"}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div [(text "#hi!" : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,1 +1,1
    -|Html_syntax.Node.div [(text "#hi!" : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [text "#hi!"]
    |}]
;;

let%expect_test "Two kinds of '#'s" =
  test {|<div>%{text "#hi!"#Module}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [(Html_syntax.Node.text (Module.to_string (text "#hi!")) : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,2 +1,1
    -|Html_syntax.Node.div
    -|  [(Html_syntax.Node.text (Module.to_string (text "#hi!")) : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [Html_syntax.Node.text (Module.to_string (text "#hi!"))]
    |}]
;;

let%expect_test "Normal case" =
  test {|<div>%{text "#hi!"#Module}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [(Html_syntax.Node.text (Module.to_string (text "#hi!")) : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,2 +1,1
    -|Html_syntax.Node.div
    -|  [(Html_syntax.Node.text (Module.to_string (text "#hi!")) : Virtual_dom.Vdom.Node.t)]
    +|Html_syntax.Node.div [Html_syntax.Node.text (Module.to_string (text "#hi!"))]
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

module%test [@name "Other contexts"] _ = struct
  let%expect_test "Tag EXPR" =
    test {|<%{f "#hi!"}></>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      f "#hi!" []
      |}]
  ;;

  let%expect_test "ATTR" =
    test {|<div %{"#hi"}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div ~attrs:[("#hi" : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      -1,1 +1,1
      -|Html_syntax.Node.div ~attrs:[("#hi" : Virtual_dom.Vdom.Attr.t)] []
      +|Html_syntax.Node.div ~attrs:["#hi"] []
      |}];
    test {|<div %{"#hi"#Foo}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div ~attrs:[(Foo.to_attr "#hi" : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      -1,2 +1,1
      -|Html_syntax.Node.div ~attrs:[(Foo.to_attr "#hi" : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|Html_syntax.Node.div ~attrs:[Foo.to_attr "#hi"] []
      |}]
  ;;

  let%expect_test "ATTR VALUE" =
    test {|<div foo=%{"#hi"}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(Html_syntax.Attr.foo "#hi" : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      -1,2 +1,1
      -|Html_syntax.Node.div
      -|  ~attrs:[(Html_syntax.Attr.foo "#hi" : Virtual_dom.Vdom.Attr.t)] []
      +|Html_syntax.Node.div ~attrs:[Html_syntax.Attr.foo "#hi"] []
      |}];
    test {|<div foo=%{"#hi"#Foo}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(Html_syntax.Attr.foo (Foo.to_string "#hi") : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      -1,3 +1,1
      -|Html_syntax.Node.div
      -|  ~attrs:[(Html_syntax.Attr.foo (Foo.to_string "#hi") : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|Html_syntax.Node.div ~attrs:[Html_syntax.Attr.foo (Foo.to_string "#hi")] []
      |}]
  ;;

  let%expect_test "Option interpolation" =
    test {|<div ?{"#hi"} ?{"#hi"#Foo}>?{"#hi"} ?{"#hi"#Foo}</div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
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

      PPX_HTML_KERNEL (diff):
      -1,12 +1,10
        Html_syntax.Node.div
      -|  ~attrs:[((match "#hi" with | None -> Html_syntax.Attr.empty | Some x -> x) :
      -|         Virtual_dom.Vdom.Attr.t);
      -|         ((match "#hi" with
      +|  ~attrs:[(match "#hi" with | None -> Html_syntax.Attr.empty | Some x -> x);
      +|         (match "#hi" with
      -|           | None -> Html_syntax.Attr.empty
      -|           | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)]
      +|          | None -> Html_syntax.Attr.empty
      +|          | Some x -> Foo.to_attr x)]
      -|  [((match "#hi" with | None -> Html_syntax.Node.none | Some x -> x) :
      -|  Virtual_dom.Vdom.Node.t);
      +|  [(match "#hi" with | None -> Html_syntax.Node.none | Some x -> x);
          Html_syntax.Node.text " ";
      -|  ((match "#hi" with
      +|  (match "#hi" with
           | None -> Html_syntax.Node.none
      -|    | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)]
      +|   | Some x -> Html_syntax.Node.text (Foo.to_string x))]
      |}]
  ;;

  let%expect_test "List interpolation" =
    test {|<div *{"#hi"} *{"#hi"#Foo}>*{"#hi"} *{"#hi"#Foo}</div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(Html_syntax.Attr.many "#hi" : Virtual_dom.Vdom.Attr.t);
               (Html_syntax.Attr.many
                  (Ppx_html_runtime.List.map "#hi" ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
        [(Html_syntax.Node.fragment "#hi" : Virtual_dom.Vdom.Node.t);
        Html_syntax.Node.text " ";
        (Html_syntax.Node.fragment
           (Ppx_html_runtime.List.map "#hi"
              ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x))) : Virtual_dom.Vdom.Node.t)]

      PPX_HTML_KERNEL (diff):
      -1,9 +1,9
        Html_syntax.Node.div
      -|  ~attrs:[(Html_syntax.Attr.many "#hi" : Virtual_dom.Vdom.Attr.t);
      -|         (Html_syntax.Attr.many
      +|  ~attrs:[Html_syntax.Attr.many "#hi";
      +|         Html_syntax.Attr.many
      -|            (Ppx_html_runtime.List.map "#hi" ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
      -|  [(Html_syntax.Node.fragment "#hi" : Virtual_dom.Vdom.Node.t);
      +|           (Ppx_html_runtime.List.map "#hi" ~f:Foo.to_attr)]
      +|  [Html_syntax.Node.fragment "#hi";
          Html_syntax.Node.text " ";
      -|  (Html_syntax.Node.fragment
      +|  Html_syntax.Node.fragment
            (Ppx_html_runtime.List.map "#hi"
      -|        ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x))) : Virtual_dom.Vdom.Node.t)]
      +|       ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x)))]
      |}]
  ;;
end
