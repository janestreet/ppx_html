open! Core
open Ppx_html_syntax.Model

class iter =
  object (self)
    inherit Traverse.iter as super

    method private mapper =
      object
        inherit Ppxlib.Ast_traverse.iter as super

        method! expression expression =
          match expression.pexp_desc with
          | Pexp_extension
              ( { txt; _ }
              , PStr
                  [ { pstr_desc =
                        Pstr_eval
                          ( { pexp_desc =
                                Pexp_constant (Pconst_string (string, loc, _delim))
                            ; _
                            }
                          , _ )
                    ; _
                    }
                  ] )
            when String.is_prefix ~prefix:"html" txt ->
            let model = Ppx_html_syntax.Model_parser.of_string ~loc string in
            let () = List.iter model ~f:(fun node -> self#node node) in
            super#expression expression
          | _ -> super#expression expression
      end

    method! ocaml_expr expr =
      let () = self#mapper#expression expr in
      super#ocaml_expr expr
  end
