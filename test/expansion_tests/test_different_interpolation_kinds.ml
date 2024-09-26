open! Core
open Test_utils

let%test_module "?{} - really basic sanity tests" =
  (module struct
    let%expect_test "Question mark - node" =
      test {|<div>?{EXPR}</div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [((match EXPR with | None -> Html_syntax.Node.none | Some x -> x) :
          Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,3 +1,2
          Html_syntax.Node.div
        -|  [((match EXPR with | None -> Html_syntax.Node.none | Some x -> x) :
        -|  Virtual_dom.Vdom.Node.t)]
        +|  [(match EXPR with | None -> Html_syntax.Node.none | Some x -> x)]
        |}]
    ;;

    let%expect_test "Question mark - attr" =
      test {|<div ?{EXPR}></div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          ~attrs:[((match EXPR with | None -> Html_syntax.Attr.empty | Some x -> x) :
                 Virtual_dom.Vdom.Attr.t)] []

        PPX_HTML_KERNEL (diff):
        -1,3 +1,3
          Html_syntax.Node.div
        -|  ~attrs:[((match EXPR with | None -> Html_syntax.Attr.empty | Some x -> x) :
        -|         Virtual_dom.Vdom.Attr.t)] []
        +|  ~attrs:[(match EXPR with | None -> Html_syntax.Attr.empty | Some x -> x)]
        +|  []
        |}]
    ;;

    let%expect_test "Question mark - node + modul" =
      test {|<div>?{EXPR#Foo}</div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [((match EXPR with
             | None -> Html_syntax.Node.none
             | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,4 +1,4
          Html_syntax.Node.div
        -|  [((match EXPR with
        +|  [(match EXPR with
              | None -> Html_syntax.Node.none
        -|     | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)]
        +|    | Some x -> Html_syntax.Node.text (Foo.to_string x))]
        |}]
    ;;

    let%expect_test "Question mark - attr + module" =
      test {|<div ?{EXPR#Foo}></div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          ~attrs:[((match EXPR with
                    | None -> Html_syntax.Attr.empty
                    | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)] []

        PPX_HTML_KERNEL (diff):
        -1,4 +1,4
          Html_syntax.Node.div
        -|  ~attrs:[((match EXPR with
        +|  ~attrs:[(match EXPR with
                     | None -> Html_syntax.Attr.empty
        -|            | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)] []
        +|           | Some x -> Foo.to_attr x)] []
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
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [(Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,2 +1,1
        -|Html_syntax.Node.div
        -|  [(Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t)]
        +|Html_syntax.Node.div [Html_syntax.Node.fragment EXPR]
        |}]
    ;;

    let%expect_test "Asterisk - attr" =
      test {|<div *{EXPR}></div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.many EXPR : Virtual_dom.Vdom.Attr.t)] []

        PPX_HTML_KERNEL (diff):
        -1,2 +1,1
        -|Html_syntax.Node.div
        -|  ~attrs:[(Html_syntax.Attr.many EXPR : Virtual_dom.Vdom.Attr.t)] []
        +|Html_syntax.Node.div ~attrs:[Html_syntax.Attr.many EXPR] []
        |}]
    ;;

    let%expect_test "Asterisk - node + modul" =
      test {|<div>*{EXPR#Foo}</div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [(Html_syntax.Node.fragment
              (Ppx_html_runtime.List.map EXPR
                 ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x))) : Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,4 +1,4
          Html_syntax.Node.div
        -|  [(Html_syntax.Node.fragment
        +|  [Html_syntax.Node.fragment
               (Ppx_html_runtime.List.map EXPR
        -|         ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x))) : Virtual_dom.Vdom.Node.t)]
        +|        ~f:(fun x -> Html_syntax.Node.text (Foo.to_string x)))]
        |}]
    ;;

    let%expect_test "Asterisk - attr + module" =
      test {|<div *{EXPR#Foo}></div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          ~attrs:[(Html_syntax.Attr.many
                     (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
          []

        PPX_HTML_KERNEL (diff):
        -1,4 +1,3
          Html_syntax.Node.div
        -|  ~attrs:[(Html_syntax.Attr.many
        +|  ~attrs:[Html_syntax.Attr.many
        -|             (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
        -|  []
        +|            (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr)] []
        |}]
    ;;
  end)
;;

let%expect_test "Asterisk with many other elements" =
  test {|<div>a *{EXPR} b</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [Html_syntax.Node.text "a ";
      (Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b"]

    PPX_HTML_KERNEL (diff):
    -1,4 +1,4
      Html_syntax.Node.div
        [Html_syntax.Node.text "a ";
    -|  (Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t);
    +|  Html_syntax.Node.fragment EXPR;
        Html_syntax.Node.text " b"]
    |}];
  test {|<div>*{EXPR} b</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [(Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b"]

    PPX_HTML_KERNEL (diff):
    -1,3 +1,2
      Html_syntax.Node.div
    -|  [(Html_syntax.Node.fragment EXPR : Virtual_dom.Vdom.Node.t);
    -|  Html_syntax.Node.text " b"]
    +|  [Html_syntax.Node.fragment EXPR; Html_syntax.Node.text " b"]
    |}]
;;

let%expect_test "Multiple asterisks" =
  test {|<div>a *{EXPR1} b *{EXPR2} *{EXPR3}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div
      [Html_syntax.Node.text "a ";
      (Html_syntax.Node.fragment EXPR1 : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " b ";
      (Html_syntax.Node.fragment EXPR2 : Virtual_dom.Vdom.Node.t);
      Html_syntax.Node.text " ";
      (Html_syntax.Node.fragment EXPR3 : Virtual_dom.Vdom.Node.t)]

    PPX_HTML_KERNEL (diff):
    -1,7 +1,7
      Html_syntax.Node.div
        [Html_syntax.Node.text "a ";
    -|  (Html_syntax.Node.fragment EXPR1 : Virtual_dom.Vdom.Node.t);
    +|  Html_syntax.Node.fragment EXPR1;
        Html_syntax.Node.text " b ";
    -|  (Html_syntax.Node.fragment EXPR2 : Virtual_dom.Vdom.Node.t);
    +|  Html_syntax.Node.fragment EXPR2;
        Html_syntax.Node.text " ";
    -|  (Html_syntax.Node.fragment EXPR3 : Virtual_dom.Vdom.Node.t)]
    +|  Html_syntax.Node.fragment EXPR3]
    |}]
;;

let%expect_test "Mixed syntaxes" =
  test
    {|<div>a *{EXPR1} b %{EXPR2} <div *{EXPR} ?{EXPR100#Bar}>?{EXPR_OPT#Foo}</div> ?{EXPR3}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
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

    PPX_HTML_KERNEL (diff):
    -1,17 +1,16
      Html_syntax.Node.div
        [Html_syntax.Node.text "a ";
    -|  (Html_syntax.Node.fragment EXPR1 : Virtual_dom.Vdom.Node.t);
    +|  Html_syntax.Node.fragment EXPR1;
        Html_syntax.Node.text " b ";
    -|  (EXPR2 : Virtual_dom.Vdom.Node.t);
    +|  EXPR2;
        Html_syntax.Node.text " ";
        Html_syntax.Node.div
    -|    ~attrs:[(Html_syntax.Attr.many EXPR : Virtual_dom.Vdom.Attr.t);
    -|           ((match EXPR100 with
    +|    ~attrs:[Html_syntax.Attr.many EXPR;
    +|           (match EXPR100 with
    -|             | None -> Html_syntax.Attr.empty
    -|             | Some x -> Bar.to_attr x) : Virtual_dom.Vdom.Attr.t)]
    -|    [((match EXPR_OPT with
    +|            | None -> Html_syntax.Attr.empty
    +|            | Some x -> Bar.to_attr x)]
    +|    [(match EXPR_OPT with
            | None -> Html_syntax.Node.none
    -|       | Some x -> Html_syntax.Node.text (Foo.to_string x)) : Virtual_dom.Vdom.Node.t)];
    +|      | Some x -> Html_syntax.Node.text (Foo.to_string x))];
        Html_syntax.Node.text " ";
    -|  ((match EXPR3 with | None -> Html_syntax.Node.none | Some x -> x) :
    -|  Virtual_dom.Vdom.Node.t)]
    +|  (match EXPR3 with | None -> Html_syntax.Node.none | Some x -> x)]
    |}]
;;

let%test_module "Using interpolation characters" =
  (module struct
    let%expect_test "%" =
      test {|<div>100%</div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text "100%"]
        |}];
      test {|<div>  %  </div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text " % "]
        |}];
      test {|<div>  100%  </div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text " 100% "]
        |}]
    ;;

    let%expect_test "?" =
      test {|<div>100?</div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text "100?"]
        |}];
      test {|<div>  ?   </div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text " ? "]
        |}];
      test {|<div>  100?   </div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text " 100? "]
        |}]
    ;;

    let%expect_test "*" =
      test {|<div>100*</div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text "100*"]
        |}];
      test {|<div>  *   </div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text " * "]
        |}];
      test {|<div>  100*   </div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text " 100* "]
        |}]
    ;;

    let%expect_test "Escaping an entire interpolation" =
      test {|<div>%%{hi}</div>|};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div [Html_syntax.Node.text "%{hi}"]
        |}];
      test {|<div>\?{hi}</div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [Html_syntax.Node.text "\\";
          ((match hi with | None -> Html_syntax.Node.none | Some x -> x) : Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,3 +1,3
          Html_syntax.Node.div
            [Html_syntax.Node.text "\\";
        -|  ((match hi with | None -> Html_syntax.Node.none | Some x -> x) : Virtual_dom.Vdom.Node.t)]
        +|  (match hi with | None -> Html_syntax.Node.none | Some x -> x)]
        |}];
      test {|<div>\*{hi}</div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [Html_syntax.Node.text "\\";
          (Html_syntax.Node.fragment hi : Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,3 +1,2
          Html_syntax.Node.div
        -|  [Html_syntax.Node.text "\\";
        -|  (Html_syntax.Node.fragment hi : Virtual_dom.Vdom.Node.t)]
        +|  [Html_syntax.Node.text "\\"; Html_syntax.Node.fragment hi]
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
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [(Html_syntax.Node.text ((EXPR)[@merlin.focus ]) : Virtual_dom.Vdom.Node.t)]

        PPX_HTML_KERNEL (diff):
        -1,2 +1,1
        -|Html_syntax.Node.div
        -|  [(Html_syntax.Node.text ((EXPR)[@merlin.focus ]) : Virtual_dom.Vdom.Node.t)]
        +|Html_syntax.Node.div [Html_syntax.Node.text ((EXPR)[@merlin.focus ])]
        |}];
      test {|<div>Hello #{EXPR}!</div>|};
      [%expect
        {|
        Difference between ppx_html and ppx_html_kernel

        PPX_HTML:
        Html_syntax.Node.div
          [Html_syntax.Node.text "Hello ";
          (Html_syntax.Node.text ((EXPR)[@merlin.focus ]) : Virtual_dom.Vdom.Node.t);
          Html_syntax.Node.text "!"]

        PPX_HTML_KERNEL (diff):
        -1,4 +1,4
          Html_syntax.Node.div
            [Html_syntax.Node.text "Hello ";
        -|  (Html_syntax.Node.text ((EXPR)[@merlin.focus ]) : Virtual_dom.Vdom.Node.t);
        +|  Html_syntax.Node.text ((EXPR)[@merlin.focus ]);
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
