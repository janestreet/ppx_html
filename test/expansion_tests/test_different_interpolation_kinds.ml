open! Core
open Test_utils

module%test [@name "?{} - really basic sanity tests"] _ = struct
  let%expect_test "Question mark - node" =
    test {|<div>?{EXPR}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [((match EXPR with | None -> Html_syntax.Node.Primitives.none | Some x -> x) :
        _)]
      |}]
  ;;

  let%expect_test "Question mark - attr" =
    test {|<div ?{EXPR}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[((match EXPR with
                  | None -> Html_syntax.Attr.Primitives.empty
                  | Some x -> x) : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[((match EXPR with
      +|  ~attrs:[(match EXPR with
                   | None -> Html_syntax.Attr.Primitives.empty
      -|            | Some x -> x) : Virtual_dom.Vdom.Attr.t)] []
      +|           | Some x -> x)] []
      |}]
  ;;

  let%expect_test "Question mark - node + modul" =
    test {|<div>?{EXPR#Foo}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [((match EXPR with
           | None -> Html_syntax.Node.Primitives.none
           | Some x -> Html_syntax.Node.Primitives.text (Foo.to_string x)) :
        _)]
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
                  | None -> Html_syntax.Attr.Primitives.empty
                  | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[((match EXPR with
      +|  ~attrs:[(match EXPR with
                   | None -> Html_syntax.Attr.Primitives.empty
      -|            | Some x -> Foo.to_attr x) : Virtual_dom.Vdom.Attr.t)] []
      +|           | Some x -> Foo.to_attr x)] []
      |}]
  ;;
end

module%test [@name "*{} - really basic sanity tests"] _ = struct
  let%expect_test "Asterisk - node" =
    test {|<div>*{EXPR}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div EXPR
      |}]
  ;;

  let%expect_test "Asterisk - attr" =
    test {|<div *{EXPR}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(Html_syntax.Attr.Primitives.many EXPR : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
      -|Html_syntax.Node.div
      -|  ~attrs:[(Html_syntax.Attr.Primitives.many EXPR : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|Html_syntax.Node.div ~attrs:[Html_syntax.Attr.Primitives.many EXPR] []
      |}]
  ;;

  let%expect_test "Asterisk - node + modul" =
    test {|<div>*{EXPR#Foo}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        (Ppx_html_runtime.List.map EXPR
           ~f:(fun x -> Html_syntax.Node.Primitives.text (Foo.to_string x)))
      |}]
  ;;

  let%expect_test "Asterisk - attr + module" =
    test {|<div *{EXPR#Foo}></div>|};
    [%expect
      {|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(Html_syntax.Attr.Primitives.many
                   (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[(Html_syntax.Attr.Primitives.many
      +|  ~attrs:[Html_syntax.Attr.Primitives.many
      -|             (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr) : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|            (Ppx_html_runtime.List.map EXPR ~f:Foo.to_attr)] []
      |}]
  ;;

  let%expect_test "Asterisk - literal before list uses cons" =
    test {|<div>%{NODE}*{CHILDREN}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div ((NODE : _) :: CHILDREN)
      |}];
    test {|<div>%{NODE1}%{NODE2}%{NODE3}*{CHILDREN}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div ((NODE1 : _) :: (NODE2 : _) :: (NODE3 : _) :: CHILDREN)
      |}]
  ;;

  let%expect_test "Asterisk - multiple interpolations use concat" =
    test {|<div>*{CHILDREN}*{MORE_CHILDREN}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div (CHILDREN @ MORE_CHILDREN)
      |}]
  ;;

  let%expect_test "Asterisk - interpolation before literal suffix uses concat" =
    test {|<div>*{CHILDREN}%{NODE}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div (CHILDREN @ [(NODE : _)])
      |}]
  ;;
end

let%expect_test "Asterisk with many other elements" =
  test {|<div>a *{EXPR} b</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div ((Html_syntax.Node.Primitives.text "a ") ::
      (EXPR @ [Html_syntax.Node.Primitives.text " b"]))
    |}];
  test {|<div>*{EXPR} b</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div (EXPR @ [Html_syntax.Node.Primitives.text " b"])
    |}]
;;

let%expect_test "Multiple asterisks - Also show that entire list is iterated through \
                 once when creating"
  =
  test {|<div>a *{EXPR1} b *{EXPR2} *{EXPR3}</div>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.div ((Html_syntax.Node.Primitives.text "a ") ::
      (EXPR1 @ ((Html_syntax.Node.Primitives.text " b ") ::
         (EXPR2 @ ((Html_syntax.Node.Primitives.text " ") :: EXPR3)))))
    |}]
;;

let%expect_test "Mixed syntaxes" =
  test
    {|<div>a *{EXPR1} b %{EXPR2} <div *{EXPR} ?{EXPR100#Bar}>?{EXPR_OPT#Foo}</div> ?{EXPR3}</div>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.div ((Html_syntax.Node.Primitives.text "a ") ::
      (EXPR1 @
         [Html_syntax.Node.Primitives.text " b ";
         (EXPR2 : _);
         Html_syntax.Node.Primitives.text " ";
         Html_syntax.Node.div
           ~attrs:[(Html_syntax.Attr.Primitives.many EXPR : Virtual_dom.Vdom.Attr.t);
                  ((match EXPR100 with
                    | None -> Html_syntax.Attr.Primitives.empty
                    | Some x -> Bar.to_attr x) : Virtual_dom.Vdom.Attr.t)]
           [((match EXPR_OPT with
              | None -> Html_syntax.Node.Primitives.none
              | Some x -> Html_syntax.Node.Primitives.text (Foo.to_string x)) :
           _)];
         Html_syntax.Node.Primitives.text " ";
         ((match EXPR3 with
           | None -> Html_syntax.Node.Primitives.none
           | Some x -> x) : _)]))

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Html_syntax.Node.div ((Html_syntax.Node.Primitives.text "a ") ::
        (EXPR1 @
           [Html_syntax.Node.Primitives.text " b ";
           (EXPR2 : _);
           Html_syntax.Node.Primitives.text " ";
           Html_syntax.Node.div
    -|       ~attrs:[(Html_syntax.Attr.Primitives.many EXPR : Virtual_dom.Vdom.Attr.t);
    -|              ((match EXPR100 with
    +|       ~attrs:[Html_syntax.Attr.Primitives.many EXPR;
    +|              (match EXPR100 with
                     | None -> Html_syntax.Attr.Primitives.empty
    -|                | Some x -> Bar.to_attr x) : Virtual_dom.Vdom.Attr.t)]
    +|               | Some x -> Bar.to_attr x)]
             [((match EXPR_OPT with
                | None -> Html_syntax.Node.Primitives.none
                | Some x -> Html_syntax.Node.Primitives.text (Foo.to_string x)) :
             _)];
           Html_syntax.Node.Primitives.text " ";
           ((match EXPR3 with
             | None -> Html_syntax.Node.Primitives.none
             | Some x -> x) : _)]))
    |}]
;;

module%test [@name "Using interpolation characters"] _ = struct
  let%expect_test "%" =
    test {|<div>100%</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "100%"]
      |}];
    test {|<div>  %  </div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "  %  "]
      |}];
    test {|<div>  100%  </div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "  100%  "]
      |}]
  ;;

  let%expect_test "?" =
    test {|<div>100?</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "100?"]
      |}];
    test {|<div>  ?   </div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "  ?   "]
      |}];
    test {|<div>  100?   </div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "  100?   "]
      |}]
  ;;

  let%expect_test "*" =
    test {|<div>100*</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "100*"]
      |}];
    test {|<div>  *   </div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "  *   "]
      |}];
    test {|<div>  100*   </div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "  100*   "]
      |}]
  ;;

  let%expect_test "Escaping an entire interpolation" =
    test {|<div>%%{hi}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div [Html_syntax.Node.Primitives.text "%{hi}"]
      |}];
    test {|<div>\?{hi}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.Primitives.text "\\";
        ((match hi with | None -> Html_syntax.Node.Primitives.none | Some x -> x) :
        _)]
      |}];
    test {|<div>\*{hi}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div ((Html_syntax.Node.Primitives.text "\\") :: hi)
      |}]
  ;;
end

module%test [@name "#{} - really basic sanity tests"] _ = struct
  let%expect_test "Hashtag mark - node" =
    test {|<div>#{EXPR}</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [(Html_syntax.Node.Primitives.text ((EXPR)[@merlin.focus ]) : _)]
      |}];
    test {|<div>Hello #{EXPR}!</div>|};
    [%expect
      {|
      same output between ppx_html and ppx_html_kernel

      Html_syntax.Node.div
        [Html_syntax.Node.Primitives.text "Hello ";
        (Html_syntax.Node.Primitives.text ((EXPR)[@merlin.focus ]) : _);
        Html_syntax.Node.Primitives.text "!"]
      |}]
  ;;

  let%expect_test "Hashtag mark - attr" =
    test_raise {|<div #{EXPR}></div>|};
    [%expect {| #{} string interpolation is not allowed in attributes |}]
  ;;

  let%expect_test "Question mark - node + module" =
    test_raise {|<div>#{EXPR#Foo}</div>|};
    [%expect {| #{} string interpolation cannot have a module identifier |}]
  ;;

  let%expect_test "Hashtag mark - attr + module" =
    test_raise {|<div #{EXPR#Foo}></div>|};
    [%expect {| #{} string interpolation is not allowed in attributes |}]
  ;;

  let%expect_test "invalid interpolation locations" =
    test_raise {|<#{EXPR}></>|};
    [%expect
      {|
      string (#{}) interpolation is not allowed here, only %{} interpolation is allowed in this context.
        |
      0 | <#{EXPR}></>
        |  ^^^^^^^ invalid interpolation here
        |
      |}];
    test_raise {|<div foo=#{EXPR}></div>|};
    [%expect
      {|
      string (#{}) interpolation is not allowed here, only %{} interpolation is allowed in this context.
        |
      0 | <div foo=#{EXPR}></div>
        |          ^^^^^^^ invalid interpolation here
        |
      |}]
  ;;

  let%expect_test "invalid interpolation locations - split across lines" =
    test_raise
      {|<#{
    EXPR
    }></>|};
    [%expect
      {|
      string (#{}) interpolation is not allowed here, only %{} interpolation is allowed in this context.
        |
      0 | <#{
        |  ^^ invalid interpolation here
      1 |     EXPR
        |
      |}];
    test_raise {|<div foo=#{EXPR}></div>|};
    [%expect
      {|
      string (#{}) interpolation is not allowed here, only %{} interpolation is allowed in this context.
        |
      0 | <div foo=#{EXPR}></div>
        |          ^^^^^^^ invalid interpolation here
        |
      |}]
  ;;

  module%test [@name "Hashtag mark - quoted string"] _ = struct
    let%expect_test "basic" =
      test {html|<div>#{{|a quoted string|}}</div>|html};
      [%expect
        {xxx|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div
          [(Html_syntax.Node.Primitives.text (({|a quoted string|})[@merlin.focus ]) :
          _)]
        |xxx}];
      test {html|<div>Hello #{{|world|}}!</div>|html};
      [%expect
        {xxx|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div
          [Html_syntax.Node.Primitives.text "Hello ";
          (Html_syntax.Node.Primitives.text (({|world|})[@merlin.focus ]) : _);
          Html_syntax.Node.Primitives.text "!"]
        |xxx}]
    ;;

    let%expect_test "with curly brace inside" =
      test {html|<div>#{{ident|curly braces } in quoted string|ident}}</div>|html};
      [%expect
        {|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div
          [(Html_syntax.Node.Primitives.text
              (({ident|curly braces } in quoted string|ident})[@merlin.focus ]) :
          _)]
        |}]
    ;;

    let%expect_test "with quote delim ender inside" =
      test {html|<div>#{{ident|curly braces |} in quoted string|ident}}</div>|html};
      [%expect
        {xxx|
        same output between ppx_html and ppx_html_kernel

        Html_syntax.Node.div
          [(Html_syntax.Node.Primitives.text
              (({ident|curly braces |} in quoted string|ident})[@merlin.focus ]) :
          _)]
        |xxx}]
    ;;
  end
end

module%test [@name "Test attr quoted string with interpolation"] _ = struct
  let%expect_test "string with %{ }" =
    test {html|<div data-test-id="empty-%{"div"}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
                   [@merlin.focus ]) ([%string "empty-%{(\"div\")}"]) : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
      +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test-id")
      -|             [@merlin.focus ]) ([%string "empty-%{(\"div\")}"]) : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|            [@merlin.focus ]) ([%string "empty-%{(\"div\")}"])] []
      |xxx}]
  ;;

  let%expect_test "string with %{ } + module path" =
    test {html|<div data-test-id="empty-%{3#Int}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
                   [@merlin.focus ]) ([%string "empty-%{(3)#Int}"]) : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
      +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test-id")
      -|             [@merlin.focus ]) ([%string "empty-%{(3)#Int}"]) : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|            [@merlin.focus ]) ([%string "empty-%{(3)#Int}"])] []
      |xxx}]
  ;;

  let%expect_test "style attr with %{ }" =
    test {html|<div style="empty-%{{|id|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[([%css "empty-%{({|id|})};"] : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[([%css "empty-%{({|id|})};"] : Virtual_dom.Vdom.Attr.t)] []
      +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ])
      +|            ([%string "empty-%{({|id|})}"])] []
      |xxx}]
  ;;

  let%expect_test "style attr with %{ } + module path" =
    test {html|<div style="empty-%{{|value#Css_gen.Length|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[([%css "empty-%{({|value#Css_gen.Length|})};"] : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[([%css "empty-%{({|value#Css_gen.Length|})};"] : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ])
      +|            ([%string "empty-%{({|value#Css_gen.Length|})}"])] []
      |xxx}]
  ;;

  let%expect_test "style attr with #{ }" =
    test {html|<div style="empty-#{{|id|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[([%css "empty-#{{|id|}};"] : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[([%css "empty-#{{|id|}};"] : Virtual_dom.Vdom.Attr.t)] []
      +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ]) "empty-#{{|id|}}"] []
      |xxx}]
  ;;

  let%expect_test "style attr with *{ }" =
    test {html|<div style="empty-*{{|id|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[([%css "empty-*{{|id|}};"] : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[([%css "empty-*{{|id|}};"] : Virtual_dom.Vdom.Attr.t)] []
      +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ]) "empty-*{{|id|}}"] []
      |xxx}]
  ;;

  let%expect_test "style attr with ?{ }" =
    test {html|<div style="empty-?{{|id|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[([%css "empty-?{{|id|}};"] : Virtual_dom.Vdom.Attr.t)] []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[([%css "empty-?{{|id|}};"] : Virtual_dom.Vdom.Attr.t)] []
      +|  ~attrs:[((Html_syntax.Attr.style)[@merlin.focus ]) "empty-?{{|id|}}"] []
      |xxx}]
  ;;

  let%expect_test "string with #{ }" =
    test {html|<div data-test-id="empty-#{{|id|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
                   [@merlin.focus ]) "empty-#{{|id|}}" : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
      +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test-id")
      -|             [@merlin.focus ]) "empty-#{{|id|}}" : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|            [@merlin.focus ]) "empty-#{{|id|}}"] []
      |xxx}]
  ;;

  let%expect_test "string with *{ }" =
    test {html|<div data-test-id="empty-*{[ {|id|} ]}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
                   [@merlin.focus ]) "empty-*{[ {|id|} ]}" : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
      +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test-id")
      -|             [@merlin.focus ]) "empty-*{[ {|id|} ]}" : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|            [@merlin.focus ]) "empty-*{[ {|id|} ]}"] []
      |xxx}]
  ;;

  let%expect_test "string with ?{ }" =
    test {html|<div data-test-id="empty-?{Some {|hd|}}"></div>|html};
    [%expect
      {xxx|
      Difference between ppx_html and ppx_html_kernel

      PPX_HTML:
      Html_syntax.Node.div
        ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
                   [@merlin.focus ]) "empty-?{Some {|hd|}}" : Virtual_dom.Vdom.Attr.t)]
        []

      PPX_HTML_KERNEL (diff):
      === DIFF HUNK ===
        Html_syntax.Node.div
      -|  ~attrs:[(((Html_syntax.Attr.Primitives.create "data-test-id")
      +|  ~attrs:[((Html_syntax.Attr.Primitives.create "data-test-id")
      -|             [@merlin.focus ]) "empty-?{Some {|hd|}}" : Virtual_dom.Vdom.Attr.t)]
      -|  []
      +|            [@merlin.focus ]) "empty-?{Some {|hd|}}"] []
      |xxx}]
  ;;
end
