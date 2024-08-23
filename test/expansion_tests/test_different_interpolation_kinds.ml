open! Core
open Test_utils

let%test_module "?{} - really basic sanity tests" =
  (module struct
    let%expect_test "Question mark - node" =
      test {|<div>?{EXPR}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [((match EXPR with | None -> Html_syntax.Node.none | Some x -> x) :
          Virtual_dom.Vdom.Node.t)]
        |}]
    ;;

    let%expect_test "Question mark - attr" =
      test {|<div ?{EXPR}></div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[((match EXPR with | None -> Html_syntax.Attr.empty | Some x -> x) :
                 Virtual_dom.Vdom.Attr.t)] []
        |}]
    ;;

    let%expect_test "Question mark - node + modul" =
      test {|<div>?{EXPR#Foo}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [((match EXPR with
             | None -> Html_syntax.Node.none
             | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)]
        |}]
    ;;

    let%expect_test "Question mark - attr + module" =
      test {|<div ?{EXPR#Foo}></div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[((match EXPR with
                    | None -> Html_syntax.Attr.empty
                    | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)] []
        |}]
    ;;
  end)
;;

let%test_module "*{} - really basic sanity tests" =
  (module struct
    let%expect_test "Asterisk - node" =
      test {|<div>*{EXPR}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [(Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t)]
        |}]
    ;;

    let%expect_test "Asterisk - attr" =
      test {|<div *{EXPR}></div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.many EXPR : Virtual_dom.Vdom.Attr.t)] []
        |}]
    ;;

    let%expect_test "Asterisk - node + modul" =
      test {|<div>*{EXPR#Foo}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [(Html_syntax.Node.fragment
              (Ppx_html_runtime.List.map EXPR
                 ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x))) : Virtual_dom.Vdom.Node.t)]
        |}]
    ;;

    let%expect_test "Asterisk - attr + module" =
      test {|<div *{EXPR#Foo}></div>|};
      [%expect
        {|
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.many
                     (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
          []
        |}]
    ;;
  end)
;;

let%expect_test "Asterisk with many other elements" =
  test {|<div>a *{EXPR} b</div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [Html_syntax.Node.text "a ";
      (Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b"]
    |}];
  test {|<div>*{EXPR} b</div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [(Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b"]
    |}]
;;

let%expect_test "Multiple asterisks" =
  test {|<div>a *{EXPR1} b *{EXPR2} *{EXPR3}</div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [Html_syntax.Node.text "a ";
      (Html_syntax.Node.fragment EXPR1 : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b ";
      (Html_syntax.Node.fragment EXPR2 : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " ";
      (Html_syntax.Node.fragment EXPR3 : Virtual_dom.Vdom.Node.t)]
    |}]
;;

let%expect_test "Mixed syntaxes" =
  test
    {|<div>a *{EXPR1} b %{EXPR2} <div *{EXPR} ?{EXPR100#Bar}>?{EXPR_OPT#Foo}</div> ?{EXPR3}</div>|};
  [%expect
    {|
    Html_syntax.Node.div
      [Html_syntax.Node.text "a ";
      (Html_syntax.Node.fragment EXPR1 : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b ";
      (EXPR2 : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " ";
      Html_syntax.Node.div
        ~attrs:[(Html_syntax.Attr.many EXPR : Virtual_dom.Vdom.Attr.t);
               ((match EXPR100 with
                 | None -> Html_syntax.Attr.empty
                 | Some x -> Bar.to_attr x) : Virtual_dom.Vdom.Attr.t)]
        [((match EXPR_OPT with
           | None -> Html_syntax.Node.none
           | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)];
      Html_syntax.Node.text " ";
      ((match EXPR3 with | None -> Html_syntax.Node.none | Some x -> x) :
      Virtual_dom.Vdom.Node.t)]
    |}]
;;

let%test_module "Using interpolation characters" =
  (module struct
    let%expect_test "%" =
      test {|<div>100%</div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text "100%"] |}];
      test {|<div>  %  </div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text " % "] |}];
      test {|<div>  100%  </div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text " 100% "] |}]
    ;;

    let%expect_test "?" =
      test {|<div>100?</div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text "100?"] |}];
      test {|<div>  ?   </div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text " ? "] |}];
      test {|<div>  100?   </div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text " 100? "] |}]
    ;;

    let%expect_test "*" =
      test {|<div>100*</div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text "100*"] |}];
      test {|<div>  *   </div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text " * "] |}];
      test {|<div>  100*   </div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text " 100* "] |}]
    ;;

    let%expect_test "Escaping an entire interpolation" =
      test {|<div>%%{hi}</div>|};
      [%expect {| Html_syntax.Node.div [Html_syntax.Node.text "%{hi}"] |}];
      test {|<div>\?{hi}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [Html_syntax.Node.text "\\";
          ((match hi with | None -> Html_syntax.Node.none | Some x -> x) : Virtual_dom.Vdom.Node.t)]
        |}];
      test {|<div>\*{hi}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [Html_syntax.Node.text "\\";
          (Html_syntax.Node.fragment hi : Virtual_dom.Vdom.Node.t)]
        |}]
    ;;
  end)
;;

let%test_module "#{} - really basic sanity tests" =
  (module struct
    let%expect_test "Hashtag mark - node" =
      test {|<div>#{EXPR}</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [(Html_syntax.Node.text ((EXPR)[@merlin.focus ]) : Virtual_dom.Vdom.Node.t)]
        |}];
      test {|<div>Hello #{EXPR}!</div>|};
      [%expect
        {|
        Html_syntax.Node.div
          [Html_syntax.Node.text "Hello ";
          (Html_syntax.Node.text ((EXPR)[@merlin.focus ]) : Virtual_dom.Vdom.Node.t);
          Html_syntax.Node.text "!"]
        |}]
    ;;

    let%expect_test "Hashtag mark - attr" =
      Expect_test_helpers_core.require_does_raise (fun () -> test {|<div #{EXPR}></div>|});
      [%expect {| ("#{} string interpolation is not allowed in attributes") |}]
    ;;

    let%expect_test "Question mark - node + modul" =
      Expect_test_helpers_core.require_does_raise (fun () ->
        test {|<div>#{EXPR#Foo}</div>|});
      [%expect {| ("#{} string intepolation cannot have a module identifier") |}]
    ;;

    let%expect_test "Hashtag mark - attr + module" =
      Expect_test_helpers_core.require_does_raise (fun () ->
        test {|<div #{EXPR#Foo}></div>|});
      [%expect {| ("#{} string interpolation is not allowed in attributes") |}]
    ;;

    let%expect_test "invalid interpolation locations" =
      Expect_test_helpers_core.require_does_raise (fun () -> test {|<#{EXPR}></>|});
      [%expect
        {| ("string (#{}) interpolation is not allowed here, only %{} interpolation is allowed in this context.") |}];
      Expect_test_helpers_core.require_does_raise (fun () ->
        test {|<div foo=#{EXPR}></div>|});
      [%expect
        {| ("string (#{}) interpolation is not allowed here, only %{} interpolation is allowed in this context.") |}]
    ;;
  end)
;;
