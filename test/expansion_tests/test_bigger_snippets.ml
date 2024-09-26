open! Core
open! Test_utils
(* NOTE: These test are less effective, but are more meant to cover a wider area of
   features instead of individually testing each feature. *)

let%expect_test "Bonsai's hello world HTML" =
  (* NOTE: This example is weird, as it's valid HTML to write

     {[
       <meta>
     ]}

     instead of:

     {[
       <meta/>
     ]}

     The same applied to <br>, <hr>, <img>, and other childless nodes. Unsure
     what is the best way of solving it. I personally think that HTML is wrong,
     and the current behavior is better. I think that as long as we provide a nice,
     descriptive error message I think I'm also happy keeping the current behavior.
  *)
  test
    {|
<html>
    <head>
        <meta charset="UTF-8" />
        <title>Hello, Bonsai!</title>
        <script defer src="main.bc.js"></script>
    </head>
    <body>
        <div id="app"></div>
    </body>
</html>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.html
      [Html_syntax.Node.head
         [Html_syntax.Node.meta
            ~attrs:[(Html_syntax.Attr.charset "UTF-8" : Virtual_dom.Vdom.Attr.t)]
            ();
         Html_syntax.Node.title [Html_syntax.Node.text "Hello, Bonsai!"];
         Html_syntax.Node.script
           ~attrs:[(Html_syntax.Attr.defer : Virtual_dom.Vdom.Attr.t);
                  (Html_syntax.Attr.src "main.bc.js" : Virtual_dom.Vdom.Attr.t)]
           []];
      Html_syntax.Node.body
        [Html_syntax.Node.div
           ~attrs:[(Html_syntax.Attr.id "app" : Virtual_dom.Vdom.Attr.t)] []]]

    PPX_HTML_KERNEL (diff):
    -1,13 +1,8
      Html_syntax.Node.html
        [Html_syntax.Node.head
    -|     [Html_syntax.Node.meta
    -|        ~attrs:[(Html_syntax.Attr.charset "UTF-8" : Virtual_dom.Vdom.Attr.t)]
    -|        ();
    +|     [Html_syntax.Node.meta ~attrs:[Html_syntax.Attr.charset "UTF-8"] ();
    -|     Html_syntax.Node.title [Html_syntax.Node.text "Hello, Bonsai!"];
    -|     Html_syntax.Node.script
    -|       ~attrs:[(Html_syntax.Attr.defer : Virtual_dom.Vdom.Attr.t);
    -|              (Html_syntax.Attr.src "main.bc.js" : Virtual_dom.Vdom.Attr.t)]
    -|       []];
    +|     Html_syntax.Node.title [Html_syntax.Node.text "Hello, Bonsai!"];
    +|     Html_syntax.Node.script
    +|       ~attrs:[Html_syntax.Attr.defer; Html_syntax.Attr.src "main.bc.js"] []];
    -|  Html_syntax.Node.body
    -|    [Html_syntax.Node.div
    -|       ~attrs:[(Html_syntax.Attr.id "app" : Virtual_dom.Vdom.Attr.t)] []]]
    +|  Html_syntax.Node.body
    +|    [Html_syntax.Node.div ~attrs:[Html_syntax.Attr.id "app"] []]]
    |}];
  (* NOTE: this is the instance that did not work (the difference is the <meta> element): *)
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|
<html>
    <head>
        <meta charset="UTF-8">
        <title>Hello, Bonsai!</title>
        <script defer src="main.bc.js"></script>
    </head>
    <body>
        <div id="app"></div>
    </body>
</html>
  |});
  [%expect {| ("Expected closing tag </meta>, got </head>") |}]
;;

let%expect_test "Highcharts example" =
  test
    {|
<html lang="en">
  <head>
    <meta charset="UTF-8"/>
  </head>
  <body>
    <h2>Examples</h2>
    <a href="simple.html">Simple</a><br/>
    <a href="simple_bonsai.html">Simple Bonsai</a><br/>
    <a href="bar_chart.html">Bar chart</a><br/>
    <a href="custom_zoom.html">Custom zoom</a><br/>
    <a href="custom_tooltip.html">Custom tooltip</a><br/>
    <a href="custom_tooltip_series.html">Custom series specific tooltip</a><br/>
    <a href="heatmap.html">Heatmap</a><br/>
    <a href="point_click_callback.html">Point click callback</a><br/>
    <a href="pie_chart.html">Pie chart</a><br/>
  </body>
</html>
  |};
  [%expect
    {|
    Difference between ppx_html and ppx_html_kernel

    PPX_HTML:
    Html_syntax.Node.html
      ~attrs:[(Html_syntax.Attr.lang "en" : Virtual_dom.Vdom.Attr.t)]
      [Html_syntax.Node.head
         [Html_syntax.Node.meta
            ~attrs:[(Html_syntax.Attr.charset "UTF-8" : Virtual_dom.Vdom.Attr.t)]
            ()];
      Html_syntax.Node.body
        [Html_syntax.Node.h2 [Html_syntax.Node.text "Examples"];
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "simple.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Simple"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "simple_bonsai.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Simple Bonsai"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "bar_chart.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Bar chart"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "custom_zoom.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Custom zoom"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "custom_tooltip.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Custom tooltip"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "custom_tooltip_series.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Custom series specific tooltip"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "heatmap.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Heatmap"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "point_click_callback.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Point click callback"];
        Html_syntax.Node.br ();
        Html_syntax.Node.a
          ~attrs:[(Html_syntax.Attr.href "pie_chart.html" : Virtual_dom.Vdom.Attr.t)]
          [Html_syntax.Node.text "Pie chart"];
        Html_syntax.Node.br ()]]

    PPX_HTML_KERNEL (diff):
    -1,44 +1,34
    -|Html_syntax.Node.html
    -|  ~attrs:[(Html_syntax.Attr.lang "en" : Virtual_dom.Vdom.Attr.t)]
    +|Html_syntax.Node.html ~attrs:[Html_syntax.Attr.lang "en"]
        [Html_syntax.Node.head
    -|     [Html_syntax.Node.meta
    -|        ~attrs:[(Html_syntax.Attr.charset "UTF-8" : Virtual_dom.Vdom.Attr.t)]
    -|        ()];
    +|     [Html_syntax.Node.meta ~attrs:[Html_syntax.Attr.charset "UTF-8"] ()];
        Html_syntax.Node.body
          [Html_syntax.Node.h2 [Html_syntax.Node.text "Examples"];
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "simple.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "simple.html"]
            [Html_syntax.Node.text "Simple"];
          Html_syntax.Node.br ();
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "simple_bonsai.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "simple_bonsai.html"]
            [Html_syntax.Node.text "Simple Bonsai"];
          Html_syntax.Node.br ();
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "bar_chart.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "bar_chart.html"]
            [Html_syntax.Node.text "Bar chart"];
          Html_syntax.Node.br ();
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "custom_zoom.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "custom_zoom.html"]
            [Html_syntax.Node.text "Custom zoom"];
          Html_syntax.Node.br ();
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "custom_tooltip.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "custom_tooltip.html"]
            [Html_syntax.Node.text "Custom tooltip"];
          Html_syntax.Node.br ();
          Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "custom_tooltip_series.html" : Virtual_dom.Vdom.Attr.t)]
    +|      ~attrs:[Html_syntax.Attr.href "custom_tooltip_series.html"]
            [Html_syntax.Node.text "Custom series specific tooltip"];
          Html_syntax.Node.br ();
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "heatmap.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "heatmap.html"]
            [Html_syntax.Node.text "Heatmap"];
          Html_syntax.Node.br ();
          Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "point_click_callback.html" : Virtual_dom.Vdom.Attr.t)]
    +|      ~attrs:[Html_syntax.Attr.href "point_click_callback.html"]
            [Html_syntax.Node.text "Point click callback"];
          Html_syntax.Node.br ();
    -|    Html_syntax.Node.a
    -|      ~attrs:[(Html_syntax.Attr.href "pie_chart.html" : Virtual_dom.Vdom.Attr.t)]
    +|    Html_syntax.Node.a ~attrs:[Html_syntax.Attr.href "pie_chart.html"]
            [Html_syntax.Node.text "Pie chart"];
          Html_syntax.Node.br ()]]
    |}]
;;
