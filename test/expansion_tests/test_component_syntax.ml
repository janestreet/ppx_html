open! Core
open! Test_utils

let%expect_test "Basic use of [F.f]" =
  test {|<Foo.f>Hihi</>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [Html_syntax.Node.Primitives.text "Hihi"]
    |}];
  test {|<Foo.f>Hihi</Foo.f>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [Html_syntax.Node.Primitives.text "Hihi"]
    |}];
  test {|<Foo.Bar.f>Hihi</>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.Bar.f [Html_syntax.Node.Primitives.text "Hihi"]
    |}];
  test {|<Foo.Bar.f>Hihi</Foo.Bar.f>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.Bar.f [Html_syntax.Node.Primitives.text "Hihi"]
    |}]
;;

let%expect_test "Basic use of [F.f] (no children)" =
  test {|<Foo.f />|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f ()
    |}];
  test {|<Foo.Bar.f />|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.Bar.f ()
    |}]
;;

let%expect_test "With a tilde!" =
  test {|<Foo.foo'></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.foo' []
    |}];
  test {|<Foo.foo'></Foo.foo'>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.foo' []
    |}];
  test {|<Foo.foo' />|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.foo' ()
    |}]
;;

let%expect_test "Mismatched closing tag" =
  test_raise {|<Foo.f></Bar.f>|};
  [%expect
    {|
    Expected closing tag </Foo.f>, but got </Bar.f>.
      |
    0 | <Foo.f></Bar.f>
      |        ^^^^^^^^ invalid closing tag here
      | ^^^^^^^ opening tag found here
      |
    |}]
;;

let%expect_test "OCaml arguments" =
  test {|<Foo.f ~foo:%{EXPR}></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [] ~foo:EXPR
    |}];
  test {|<Foo.f ~foo:%{EXPR} ?bam ?beep:%{EXPR_BEEP 1} ~boop></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [] ~foo:EXPR ?bam ?beep:(EXPR_BEEP 1) ~boop
    |}];
  test {|<Foo.f ~punned></>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f [] ~punned
    |}];
  test
    {|<Foo.f
          ~foo:%{EXPR}
          ~bar:%{EXPR2}
          attr1=%{EXPR}
          bar=but-as-an-attr
          ~bam
        ></>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Foo.f
      ~attrs:[(((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr" :
             Virtual_dom.Vdom.Attr.t)] [] ~foo:EXPR ~bar:EXPR2 ~bam

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Foo.f
    -|  ~attrs:[(((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR;
    -|         (((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr" :
    -|         Virtual_dom.Vdom.Attr.t)] [] ~foo:EXPR ~bar:EXPR2 ~bam
    +|         ((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr"] []
    +|  ~foo:EXPR ~bar:EXPR2 ~bam
    |}];
  test
    {|<%{component}
        ~foo:%{EXPR}
        ~bar:%{EXPR2}
        attr1=%{EXPR}
        bar=but-as-an-attr
        ~bam
      ></>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    component
      ~attrs:[(((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr" :
             Virtual_dom.Vdom.Attr.t)] [] ~foo:EXPR ~bar:EXPR2 ~bam

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      component
    -|  ~attrs:[(((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR;
    -|         (((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr" :
    -|         Virtual_dom.Vdom.Attr.t)] [] ~foo:EXPR ~bar:EXPR2 ~bam
    +|         ((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr"] []
    +|  ~foo:EXPR ~bar:EXPR2 ~bam
    |}]
;;

let%expect_test "Bonsai view expansion" =
  test
    {|<View.hbox ~gap:%{`Rem 1.0}>
        <Box.component />
        <Box.component />
        <Box.component />
        <Box.component />
      </>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    View.hbox
      [Box.component (); Box.component (); Box.component (); Box.component ()]
      ~gap:(`Rem 1.0)
    |}]
;;

let%expect_test "Double attr" =
  test
    {|<Foo.f
          ~foo:%{EXPR}
          ~bar:%{EXPR2}
          ~attr:%{[]}
          attr1=%{EXPR}
          bar=but-as-an-attr
          ~bam
        ></>|};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Foo.f
      ~attrs:[(((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR : Virtual_dom.Vdom.Attr.t);
             (((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr" :
             Virtual_dom.Vdom.Attr.t)] [] ~foo:EXPR ~bar:EXPR2 ~attr:[] ~bam

    PPX_HTML_KERNEL (diff):
    === DIFF HUNK ===
      Foo.f
    -|  ~attrs:[(((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR : Virtual_dom.Vdom.Attr.t);
    +|  ~attrs:[((Html_syntax.Attr.attr1)[@merlin.focus ]) EXPR;
    -|         (((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr" :
    -|         Virtual_dom.Vdom.Attr.t)] [] ~foo:EXPR ~bar:EXPR2 ~attr:[] ~bam
    +|         ((Html_syntax.Attr.bar)[@merlin.focus ]) "but-as-an-attr"] []
    +|  ~foo:EXPR ~bar:EXPR2 ~attr:[] ~bam
    |}]
;;

let%expect_test "Component shorthand syntax" =
  test
    {|
    <>
      <Foo></>
      <Foo></Foo>
      <Foo />
    </>
  |};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Html_syntax.Node.Primitives.fragment
      [Foo.component' []; Foo.component' []; Foo.component' ()]
    |}]
;;

let%expect_test "Component syntax support OCaml args - literal result in nice errors" =
  test_raise
    {|
    <>
      <Foo.f ~arg1:foo></>
    </>
  |};
  [%expect
    {|
    Expected an OCaml interpolation (e.g. %{}) or HTML element (e.g. (<></>))
      |
    1 |     <>
    2 |       <Foo.f ~arg1:foo></>
      |                    ^ invalid argument here
    3 |     </>
      |
      | Hint: Did you mean to write %{...}? Bare identifiers are not allowed here, wrap the expression in %{...}.
    |}];
  test_raise
    {|
    <>
      <Foo.f ~arg1:"foo"></>
    </>
  |};
  [%expect
    {|
    Expected an OCaml interpolation (e.g. %{}) or HTML element (e.g. (<></>))
      |
    1 |     <>
    2 |       <Foo.f ~arg1:"foo"></>
      |                    ^ invalid argument here
    3 |     </>
      |
    |}]
;;

let%expect_test "List splice (*{}) in component children passes list directly" =
  test {|<Foo.f>*{contents}</>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f contents
    |}];
  test {|<Foo.f><Bar.b>Hello</> *{more_contents}</>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f ((Bar.b [Html_syntax.Node.Primitives.text "Hello"]) ::
      (Html_syntax.Node.Primitives.text " ") :: more_contents)
    |}];
  test {|<Foo.f><Bar.b>Hello</> <div>Goodbye</div> *{more_contents}</>|};
  [%expect
    {|
    same output between ppx_html and ppx_html_kernel

    Foo.f ((Bar.b [Html_syntax.Node.Primitives.text "Hello"]) ::
      (Html_syntax.Node.Primitives.text " ") ::
      (Html_syntax.Node.div [Html_syntax.Node.Primitives.text "Goodbye"]) ::
      (Html_syntax.Node.Primitives.text " ") :: more_contents)
    |}]
;;
