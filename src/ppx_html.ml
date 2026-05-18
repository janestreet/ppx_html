open! Core

let () =
  Ppxlib.Driver.register_transformation
    "ppx_html"
    ~extensions:
      [ Ppx_html_expander.Extension.extension
          ~name:"ppx_html.html"
          ~runtime_kind:Js_of_ocaml
          ~experimental_features_allowed:false
          ~skip_whitespace_behavior_check:false
      ; Ppx_html_expander.Extension.extension
          ~name:"ppx_html.html.jsx"
          ~runtime_kind:Js_of_ocaml
          ~experimental_features_allowed:false
          ~skip_whitespace_behavior_check:true
      ; Ppx_html_expander.Extension.extension
          ~name:"ppx_html.html_experimental"
          ~runtime_kind:Js_of_ocaml
          ~experimental_features_allowed:true
          ~skip_whitespace_behavior_check:false
      ; Ppx_html_expander.Extension.extension
          ~name:"ppx_html.html_experimental.jsx"
          ~runtime_kind:Js_of_ocaml
          ~experimental_features_allowed:true
          ~skip_whitespace_behavior_check:true
      ]
;;
