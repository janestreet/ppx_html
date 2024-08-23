PPX_HTML
========

<link rel="stylesheet" href="../ppx_css/readme-style.css">

`ppx_html` is a PPX that lets you write HTML inside of OCaml 🐪 programs. It is
spiritually similar to [JSX](<https://en.wikipedia.org/wiki/JSX_(JavaScript)>).

(the type annotations are unnecessary and only for educational purposes.)

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

```ocaml
let open Virtual_dom_svg.Html_syntax in
{%html|
<svg height="100" width="100">
  <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />
</svg>
|}
```

Alternatively, you can:

```ocaml
[%html.Virtual_dom_svg {|
<svg height="100" width="100">
  <circle cx="50" cy="50" r="40" stroke="black" stroke-width="3" fill="red" />
</svg>
|}]

```

You can go back to using `virtual_dom` nodes by opening `Virtual_dom.Html_syntax`.
Opening `Bonsai_web` does this for you!
