PPX_HTML
========

`ppx_html` is a PPX that lets you write HTML inside of OCaml 🐪 programs. It is
spiritually similar to [JSX](<https://en.wikipedia.org/wiki/JSX_(JavaScript)>).

(the type annotations are unnecessary and only for educational purposes.)

<!-- $MDX skip -->
```ocaml
{%html|
  <div %{centered : Vdom.Attr.t}>
    <p>Capybaras are the world's largest living rodent.</p>
    <br />
    <img style="width: 50%" src=%{image_url : string} />
    %{description : Vdom.Node.t}
  </div>
|}
```

is equivalent to:

<!-- $MDX skip -->
```ocaml
Vdom.Node.div
  ~attrs:[ centered ]
  [ Vdom.Node.p [ Vdom.Node.text "Capybaras are the world's largest living rodent." ]
  ; Vdom.Node.br ()
  ; Vdom.Node.img ~attrs:[ {%css|width: 50%|}; Vdom.Attr.src image_url ] ()
  ; description
  ]
```

To use it in your project, add `ppx_html` to your jbuild's preprocess field:

```lisp
(preprocess (pps (ppx_jane ppx_html)))
```

Auto-formatting should happen by default.

VIM and VS Code should syntax highlight it by default. You can enable Emacs syntax
highlighting by adding `(Jane.polymode)` to your Emacs config. Eventually we want to make
syntax highlighting always happen by default in emacs.

Node Syntax
-----------

`ppx_html`'s syntax is similar to HTML's. To embed OCaml values, use `ppx_string`'s
familiar syntax:

| Syntax                             | Description                                                                              |
| -----------                        | -----------                                                                              |
| `{%html|<div>%{EXPR}</div>|}`      | `EXPR` is expected to be `Vdom.Node.t`                                                   |
| `{%html|<div>%{EXPR#Foo}</div>|}`  | Similar to ppx_string, will call [Vdom.Node.text (Foo.to_string EXPR)]                   |
| `{%html|<div>*{EXPR}</div>|}`      | `EXPR` is expected to be `Vdom.Node.t list`                                              |
| `{%html|<div>?{EXPR}</div>|}`      | `EXPR` is expected to be `Vdom.Node.t option`                                            |
| `{%html|<div>#{EXPR}</div>|}`      | `EXPR` is expected to be `string`                                                        |
| `{%html|<div>%{"a string"}</div>|}`| Will call [ Vdom.Node.text "a string" ]                                                  |
| `<TAG ATTRS...> INNER </TAG>`      | TAG must be `Vdom.Node.TAG : ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Vdom.Node.t` |
| `<TAG ATTRS.../>`                  | TAG must be `Vdom.Node.TAG : ?attrs:Vdom.Attr.t list -> unit -> Vdom.Node.t`             |
| `<div %{EXPR} > INNER </div>`      | `EXPR` must be `Attr.t`                                                                  |
| `<div ?{EXPR} > INNER </div>`      | `EXPR` must be `Attr.t option`                                                           |
| `<div *{EXPR} > INNER </div>`      | `EXPR` must be `Attr.t list`                                                             |
| `<%{TAGEXPR} ATTRS...> INNER </>`  | Where `TAGEXPR : ?attrs:Vdom.Attr.t list -> Vdom.Node.t list -> Vdom.Node.t`             |
| `<%{TAGEXPR} ATTRS.../>`           | Where `TAGEXPR : ?attrs:Vdom.Attr.t list -> unit -> Vdom.Node.t`                         |
| `<Foo.f> INNER </>`                | Sugar for `<%{Foo.f}> INNER </>`.                                                        |
| `<Foo.f> INNER </Foo.f>`           | Alternate syntax for `<Foo.f> INNER </>`.                                                |
| `<Foo.f ~foo:%{EXPR}></>`          | Passes ~foo:EXPR to Foo.f as an OCaml argument (also supports ?optional arguments).      |
| `<Foo.f ~foo></>`                  | Shorthand for `<Foo.f ~foo:%{foo}></>` (also supports ?optional arguments).              |
| `{%html|<></>|}`                   | Will call `Vdom.Node.fragment`.                                                          |

Attribute Syntax
----------------

Nodes may have ATTRS as described below:

- `NAME` then NAME is `Vdom.Attr.NAME : Vdom.Attr.t`.
- `NAME=VALUE`  then NAME is a name in `Vdom.Attr.NAME : 'a -> Vdom.Attr.t` and `VALUE` is one of
    - `UNQUOTEDLITERAL` - treated as a string. There is a heuristic to parse the string
      into the correct `'a`.
    - `"QUOTED_LITERAL"` - treated as a string, but allows `ppx_string` interpolation,
      and will trigger similar heuristics to parse the string.
      In particular `style="..."` and `style=...` will use `ppx_css`.
      Additionally, `tailwind="..."` and `tailwind=...` will use `ppx_tailwind`.
    - `%{EXPR}` - arbitrary ocaml expression that should evaluate the the `'a` that is
      expected.
- `key=VALUE` will pass ~key.

Additionally some OCaml keywords that are also attributes (e.g. `for`) are special cased
to expand to `Vdom.Attr.for_`.

How can I use tailwind?
-----------------------
To use `ppx_tailwind`, , we've special cased a "tailwind" attribute `<div
tailwind="..."></div>` behaves like `<div %{[%tailwind ".."]}></div>`.


How can I use `Virtual_dom_svg`?
------------------------------

To create `virtual_dom_svg` nodes instead of `virtual_dom_svg`, open
`Virtual_dom_svg`'s `Html_syntax`:


<!-- $MDX file=test/vdom_tests/test_vdom_svg_with_hyphens.ml,part=open-example -->
```ocaml
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
```

Alternatively, you can:

<!-- $MDX file=test/vdom_tests/test_vdom_svg_with_hyphens.ml,part=inline-example -->
```ocaml
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
```

You can go back to using `virtual_dom` nodes by opening `Virtual_dom.Html_syntax`.
Opening `Bonsai_web` does this for you!

How can I use `ppx_html` outside of a Javascript context (e.g. to perform server-side rendering of HTML)?
---------------------------------------------------------------------------------------------------------
You can use `ppx_html_kernel`. It is a version of `ppx_html` without JavaScript
dependencies and without `js_of_ocaml` assumptions (e.g. it does not attempt to use
`ppx_css`).

There is an `Html_syntax` for `lib/html` in the library `ppx_html_lib_html_syntax`. To use it, you must:
1. Add the library `ppx_html_lib_html_syntax` as a dependency
2. Add the ppx `ppx_html_kernel` (different from `ppx_html`!) as a dependency to your preprocess field.
3. After that you can use `ppx_html_kernel` by opening `ppx_html_lib_html_syntax`:

<!-- $MDX skip -->
```ocaml
open! Core
open Ppx_html_lib_html_syntax.Html_syntax

let hello = 
    {%html|
        <html>
          <head>
            <style>
              body {
                background-color: tomato;
              }
            </style>
          </head>
          <body>
            <div><h1>Hello!!</h1></div>
          </body>
        </html>
      |};
```

Note that there are some differences. `ppx_html` special cases `style=` into using `ppx_css` while `ppx_html_kernel`
does not, which means that using nested CSS inside of style tags (e.g. `&:hover body {}` will not do what you expect
in `ppx_html_kernel`).
