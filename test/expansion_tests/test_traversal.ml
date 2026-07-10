open! Core

let%expect_test "Traversal can reach nested ppx_html expressions" =
  let expr =
    Ppx_html_syntax.Model_parser.of_string
      ~loc:Ppxlib.Location.none
      (* The following is laid out in a way to show how many nodes there should be in the
         final output *)
      {|
      <div>
      Outer statement 
      %{[%html {xxx|<div>Inner statement 1</div>|xxx}]}
      %{[%html {xxx|<span>Inner statement 2 %{[%html {yyy|<span>Inner statement 3, nested in 2</span>|yyy}]} </span>|xxx}]}
      </div>
    |}
  in
  let all_nodes = ref [] in
  let traverser =
    object
      inherit Ppx_html_traverser.iter as super

      method! node node =
        all_nodes := node :: !all_nodes;
        super#node node
    end
  in
  let () = List.iter ~f:traverser#node expr in
  let node_expr =
    !all_nodes
    |> List.rev
    |> List.map ~f:(fun node ->
      Ppx_html_expander.Model_code_gen.code
        ~html_syntax_module:None
        ~loc:Ppxlib.Location.none
        ~runtime_kind:Js_of_ocaml
        [ node ]
      |> Ppxlib.Pprintast.string_of_expression)
  in
  let string_expr = String.concat ~sep:"\n\n===========================\n\n" node_expr in
  print_endline string_expr;
  (* We should see both the literal ppx node as well as the inner content of the node in
     the list below. Currently, we only see the literal ppx nodes for nested statements *)
  [%expect
    {|
    Html_syntax.Node.div
      [Html_syntax.Node.Primitives.text "Outer statement";
      ([%html {xxx|<div>Inner statement 1</div>|xxx}] : _);
      ([%html
         {xxx|<span>Inner statement 2 %{[%html {yyy|<span>Inner statement 3, nested in 2</span>|yyy}]} </span>|xxx}] :
      _)]

    ===========================

    Html_syntax.Node.Primitives.text "Outer statement"

    ===========================

    ([%html {xxx|<div>Inner statement 1</div>|xxx}] : _)

    ===========================

    Html_syntax.Node.div [Html_syntax.Node.Primitives.text "Inner statement 1"]

    ===========================

    Html_syntax.Node.Primitives.text "Inner statement 1"

    ===========================

    ([%html
       {xxx|<span>Inner statement 2 %{[%html {yyy|<span>Inner statement 3, nested in 2</span>|yyy}]} </span>|xxx}] :
    _)

    ===========================

    Html_syntax.Node.span
      [Html_syntax.Node.Primitives.text "Inner statement 2 ";
      ([%html {yyy|<span>Inner statement 3, nested in 2</span>|yyy}] : _);
      Html_syntax.Node.Primitives.text " "]

    ===========================

    Html_syntax.Node.Primitives.text "Inner statement 2 "

    ===========================

    ([%html {yyy|<span>Inner statement 3, nested in 2</span>|yyy}] : _)

    ===========================

    Html_syntax.Node.span
      [Html_syntax.Node.Primitives.text "Inner statement 3, nested in 2"]

    ===========================

    Html_syntax.Node.Primitives.text "Inner statement 3, nested in 2"

    ===========================

    Html_syntax.Node.Primitives.none
    |}]
;;
