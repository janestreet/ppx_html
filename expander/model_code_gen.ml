open! Core
open! Ppxlib
open Ppx_html_syntax
open Model
module C = Ast_builder.Default

let sanitize_ocaml_keyword s = if Ppxlib.Keyword.is_keyword s then s ^ "_" else s
let is_capitalized s = (not (String.is_empty s)) && Char.is_uppercase s.[0]

let rec node_expr
  :  html_syntax_module:longident loc option -> runtime_kind:Runtime_kind.t -> Node.t
  -> expression
  =
  fun ~html_syntax_module ~runtime_kind -> function
  | Text { txt; loc } ->
    [%expr
      [%e Shared.node_fn ~loc ~html_syntax_module ~primitive:true "text"]
        [%e C.estring ~loc txt]]
  | Expr { expr; interpolation_kind } ->
    Expr_code_gen.expr
      ~html_syntax_module
      ~runtime_kind
      ~type_:(Node { interpolation_kind })
      expr
  | Element elmt -> element_expr ~html_syntax_module ~runtime_kind elmt

and argument_expr
  ~name
  ~argument
  ~(sigil : Model.Attr.Sigil.t)
  ~runtime_kind
  ~html_syntax_module
  =
  let label =
    match sigil with
    | Tilde -> Labelled name.txt
    | Question_mark -> Optional name.txt
  and expr =
    match argument with
    | None -> Ast_helper.Exp.ident ~loc:name.loc { txt = Lident name.txt; loc = name.loc }
    | Some argument ->
      (match argument with
       | Model.Attr.Argument.Expr expr ->
         Expr_code_gen.expr ~runtime_kind ~html_syntax_module ~type_:Argument expr
       | Model.Attr.Argument.Element elmt ->
         element_expr ~runtime_kind ~html_syntax_module elmt)
  in
  label, expr

and element_expr
  ~html_syntax_module
  ~runtime_kind
  ({ tag
   ; attrs
   ; inner
   ; loc = full_loc
   ; open_loc = loc
   ; open_string_relative_location = _
   ; closing_tag = _
   } :
    Element.t)
  =
  let tag =
    match tag with
    | Literal (Literal name) ->
      Shared.node_fn
        ~loc:name.loc
        ~html_syntax_module
        ~primitive:false
        (sanitize_ocaml_keyword name.txt)
    | Literal (Component { name; string_relative_location; code }) ->
      let expr =
        let expr =
          let name =
            match name.txt with
            | (Lident x | Ldot (_, x)) when is_capitalized x ->
              { name with txt = Ldot (name.txt, "component'") }
            | _ -> name
          in
          Ppxlib.Ast_builder.Default.pexp_ident ~loc:name.loc name
        in
        { Expr.expr
        ; string_relative_location
        ; code
        ; to_t = None
        ; loc = code.loc
        ; escape_kind = Not_escaped
        }
      in
      Expr_code_gen.expr ~runtime_kind ~html_syntax_module expr
    | Expr e -> Expr_code_gen.expr ~runtime_kind ~html_syntax_module e
    | Fragment loc -> Shared.node_fn ~loc ~html_syntax_module ~primitive:true "fragment"
  in
  let attrs, keys =
    List.partition_map attrs ~f:(function
      | Attr.Attr { name = { txt = "key"; loc = _ }; value; loc } -> Second (value, loc)
      | x -> First x)
  in
  let key_args =
    match keys with
    | [] -> []
    | _ :: (_, second_loc) :: _ ->
      Location.raise_errorf
        ~loc:second_loc
        {|Error: There can only be one key. Please remove this duplicate key.|}
    | [ (None, loc) ] ->
      Location.raise_errorf
        ~loc
        {|Error: The attribute key needs a value. (e.g. key=a-unique-key)|}
    | [ (Some value, _) ] ->
      let arg_expression =
        Attr_code_gen.value_to_expression ~runtime_kind ~html_syntax_module value
      in
      [ Labelled "key", arg_expression ]
  in
  let attrs, arguments =
    List.partition_map attrs ~f:(function
      | Attr.Expr { expr; interpolation_kind } ->
        let result =
          Expr_code_gen.expr
            ~runtime_kind
            ~html_syntax_module
            ~type_:(Attr { interpolation_kind })
            expr
        in
        Either.First result
      | Argument { name; argument; sigil; loc = _ } ->
        let result =
          argument_expr ~name ~argument ~sigil ~runtime_kind ~html_syntax_module
        in
        Second result
      | Attr { name; value = None; loc = _ } ->
        let result =
          Shared.attr_fn
            ~loc:name.loc
            ~html_syntax_module
            ~primitive:false
            (sanitize_ocaml_keyword name.txt)
        in
        First result
      | Attr { name; value = Some value; loc } ->
        let result =
          Attr_code_gen.code ~runtime_kind ~loc ~html_syntax_module name value
        in
        First result)
  in
  let args =
    let attrs =
      let maybe_enforce_type expressions =
        match runtime_kind with
        | Js_of_ocaml ->
          List.map expressions ~f:(fun e ->
            [%expr ([%e e] : [%t Shared.attr_t_type ~loc])])
        | Kernel -> expressions
      in
      if List.is_empty attrs
      then []
      else [ Labelled "attrs", attrs |> maybe_enforce_type |> C.elist ~loc ]
    in
    let nodes =
      [ ( Nolabel
        , match inner with
          | None -> [%expr ()]
          | Some inner ->
            (* This branch __should__ include the empty list as [None] is for self-closing
               tags *)
            let loc =
              (* Any [ppx_metaquot] expressions that are created from this point forward
                 will use [loc] for their location. Retrieve the location of every
                 expression within [inner] *)
              match inner with
              | [] -> full_loc
              | first :: _ ->
                let last = List.last_exn inner in
                let first, last = Node.loc first, Node.loc last in
                { loc_start = first.loc_start; loc_end = last.loc_end; loc_ghost = false }
            in
            let expr =
              List.fold_right inner ~init:None ~f:(fun node acc ->
                let node_expr =
                  match node with
                  | Node.Expr { interpolation_kind = List; expr; _ } ->
                    `List
                      (Expr_code_gen.node_list_expr
                         ~html_syntax_module
                         ~runtime_kind
                         expr)
                  | node -> `Node (node_expr ~html_syntax_module ~runtime_kind node)
                in
                match node_expr, acc with
                | `List node_expr, None ->
                  (* No other expression yet, this list should be the starting list *)
                  Some node_expr
                | `List node_expr, Some acc ->
                  (* The [@] and [::] operators are right-associative, meaning they will
                     process everything to the right of it before processing itself.

                     This means that each append should only run once, meaning that every
                     element should only be processed once so long as we're doing the cons
                     and appends in a single statement and not in a loop.

                     Since 5.1 it's also tail-recursive

                     https://ocaml.org/manual/5.1/api/Stdlib.html#:~:text=val%20(%40)%20%3A%20%27a%20list%20%2D%3E%20%27a%20list%20%2D%3E%20%27a%20list *)
                  Some [%expr [%e node_expr] @ [%e acc]]
                | `Node node_expr, None -> Some [%expr [ [%e node_expr] ]]
                | `Node node_expr, Some acc ->
                  (* [::] is the same as defining the elements directly within a list *)
                  Some [%expr [%e node_expr] :: [%e acc]])
            in
            (match expr with
             | Some expr -> expr
             | None ->
               (* Starting with a [None] instead of an empty list expression so that we
                  don't append onto an empty list if the last element in the [inner]
                  expression is a list, which would be an unnecessary runtime performance
                  regression *)
               [%expr []]) )
      ]
    in
    List.concat [ key_args; attrs; nodes; arguments ]
  in
  C.pexp_apply ~loc:full_loc tag args
;;

let code ~loc ~html_syntax_module ~(runtime_kind : Runtime_kind.t) (model : Node.t list) =
  let model =
    List.filter model ~f:(function
      | Text { txt; _ } when String.for_all txt ~f:Char.is_whitespace -> false
      | _ -> true)
  in
  match model with
  | [] -> Shared.node_fn ~html_syntax_module ~loc ~primitive:true "none"
  | [ t ] -> { (node_expr ~html_syntax_module ~runtime_kind t) with pexp_loc = loc }
  | _ :: _ as elements ->
    Location.raise_errorf
      ~loc
      "ppx_html expects to return a single html element, but found %d top-level elements."
      (List.length elements)
;;
