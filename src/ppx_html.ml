open! Core

let () =
  Ppxlib.Driver.register_transformation
    "ppx_html"
    ~extensions:
      [ Ppx_html_expander.Extension.extension
          ~name:"ppx_html.html"
          ~runtime_kind:Js_of_ocaml
      ]
;;
