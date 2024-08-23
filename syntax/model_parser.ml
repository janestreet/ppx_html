open Core
open Ppxlib
open Angstrom
open Angstrom.Let_syntax
open Model

module Locations : sig
  type t

  val create : Location.t -> string -> t
  val location : t -> start:int -> end_:int -> Location.t
end = struct
  type t =
    { lines : position Int.Map.t
    ; text : string
    }

  let position ~lines i =
    let i', pos = Map.closest_key lines `Less_or_equal_to i |> Option.value_exn in
    { pos with pos_cnum = pos.pos_cnum - i' + i }
  ;;

  let create { loc_start; _ } text =
    let lines = Int.Map.empty in
    let lines = Map.set lines ~key:0 ~data:loc_start in
    let lines =
      String.foldi text ~init:lines ~f:(fun i lines c ->
        if Char.( <> ) c '\n'
        then lines
        else (
          let pos =
            { loc_start with
              pos_cnum = loc_start.pos_cnum + i + 1
            ; pos_bol = loc_start.pos_cnum + i + 1
            ; pos_lnum = loc_start.pos_lnum + Map.length lines
            }
          in
          Map.set lines ~key:(i + 1) ~data:pos))
    in
    { lines; text }
  ;;

  let position { lines; text } i =
    assert (0 <= i && i <= String.length text);
    position ~lines i
  ;;

  let location t ~start ~end_ =
    let loc_start = position t start
    and loc_end = position t end_ in
    { Location.loc_start; loc_end; loc_ghost = false }
  ;;
end

module Error = struct
  module Position = struct
    type t = Lexing.position =
      { pos_fname : string
      ; pos_lnum : int
      ; pos_bol : int
      ; pos_cnum : int
      }
    [@@deriving sexp]
  end

  module Location = struct
    include Location

    type t = Location.t =
      { loc_start : Position.t
      ; loc_end : Position.t
      ; loc_ghost : bool
      }
    [@@deriving sexp]
  end

  module T = struct
    type t =
      { message : string
      ; location : Location.t option
      }
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let of_string str =
    try of_string str with
    | _ -> { location = None; message = str }
  ;;

  let raise ~loc { location; message } =
    Location.raise_errorf ~loc:(Option.value location ~default:loc) "%s" message
  ;;
end

let ocaml_expression ({ txt; loc } : string Loc.t) =
  (* borrowed from [ppx_string] *)
  let lexbuf = Lexing.from_string txt in
  lexbuf.lex_abs_pos <- loc.loc_start.pos_cnum;
  lexbuf.lex_curr_p <- loc.loc_start;
  Parse.expression lexbuf
;;

let with_loc' ~locs f =
  let%map start = pos
  and f
  and end_ = pos in
  let loc = Locations.location locs ~start ~end_ in
  f loc
;;

let with_loc ~locs txt =
  with_loc'
    ~locs
    (let%map txt in
     fun loc -> { txt; loc })
;;

let fail_but_we_know_location ~exn ~loc message =
  let message =
    assert (String.is_empty (Format.flush_str_formatter ()));
    Format.fprintf Format.str_formatter "%s\n" message;
    Location.report_exception Format.str_formatter exn;
    Format.flush_str_formatter ()
  in
  fail (Error.to_string { location = Some loc; message })
;;

let fail ?start ?end_ ~locs ?exn message =
  let%bind pos in
  let start = Option.value start ~default:pos in
  let end_ = Option.value end_ ~default:pos in
  let location = Locations.location locs ~start ~end_ in
  let message =
    match exn with
    | None -> message
    | Some exn ->
      assert (String.is_empty (Format.flush_str_formatter ()));
      Format.fprintf Format.str_formatter "%s\n" message;
      Location.report_exception Format.str_formatter exn;
      Format.flush_str_formatter ()
  in
  fail (Error.to_string { location = Some location; message })
;;

let string s =
  let%map (_ : string) = string s in
  ()
;;

let char c =
  let%map (_ : char) = char c in
  ()
;;

let option s = option None (s >>| Option.some)

let scan_through_ocaml_expression_until_unclosed_curly_brace ~locs =
  let%bind code =
    scan_string "" (fun curr_string c ->
      (* NOTE: This code is semi-complex/ugly. What it does is adding support for
         being able to handle tricky edge cases like:

         [%html {|<div>%{Vdom.Node.text "}"}</div>|}]

         Solely reading a single character at a time results in the curly brace inside of
         the interpolated ocaml closing the entire segment despite it being inside
         of an escaped string.

         The way this code works is that it solely calls the OCaml tokenizer until
         it seems a string with an unclosed curly brace, at which point it stops 
         scanning the string. *)
      let curr_string = curr_string ^ Char.to_string c in
      let tokens_of_string = Ocaml_parsing.string_tokens curr_string in
      match tokens_of_string with
      | Error _ -> Some curr_string
      | Ok tokens ->
        let has_unclosed_curlys =
          List.fold tokens ~init:0 ~f:(fun parens -> function
            | LBRACE -> parens + 1
            | RBRACE -> parens - 1
            | _ -> parens)
          < 0
        in
        (match has_unclosed_curlys with
         | true -> None
         | false -> Some curr_string))
  in
  match%bind peek_char with
  | Some '}' -> return code
  | None | Some _ -> fail ~locs "Missing curly brace for interpolated OCaml"
;;

let identify_case_if_string_is_peeked ~string ~case =
  match%bind peek_string (String.length string) with
  | peeked when String.equal peeked string -> return case
  | _ -> Angstrom.fail "If you ever see this error message, this is a ppx_html bug"
;;

let identify_case_if_next_char_matches ~test ~case =
  match%bind peek_char with
  | Some peeked when test peeked -> return case
  | _ -> Angstrom.fail "If you ever see this error message, this is a ppx_html bug"
;;

let ( => ) string case = identify_case_if_string_is_peeked ~string ~case
let ( >=> ) test case = identify_case_if_next_char_matches ~test ~case

let parse_intepolation_kind : Model.Interpolation_kind.t t =
  choice
    [ string "%{" *> return Model.Interpolation_kind.Normal
    ; string "?{" *> return Model.Interpolation_kind.Option
    ; string "*{" *> return Model.Interpolation_kind.List
    ; string "#{" *> return Model.Interpolation_kind.String
    ]
;;

type interpolation =
  { expr : Model.Expr.t
  ; interpolation_kind : Model.Interpolation_kind.t
  }

let parse_expr_common ~locs : interpolation Angstrom.t =
  with_loc'
    ~locs
    (let%bind interpolation_kind = parse_intepolation_kind
     and start = pos
     and code = scan_through_ocaml_expression_until_unclosed_curly_brace ~locs
     and end_ = pos
     and () = string "}" in
     let code, end_, to_t =
       match Ocaml_parsing.rsplit_on_hash code with
       | None -> code, end_, None
       | Some (code, to_t) ->
         let length = String.length to_t in
         let to_t =
           { txt = to_t
           ; loc = Locations.location locs ~start:(end_ - String.length to_t - 1) ~end_
           }
           |> ocaml_expression
           |> function
           | { pexp_desc = Pexp_construct (t, None); _ } ->
             { t with txt = Longident.name t.txt }
           | { pexp_loc; _ } ->
             Location.raise_errorf ~loc:pexp_loc "Expected a module identifier (e.g. Foo)"
         in
         code, end_ - length - 1, Some to_t
     in
     let code =
       { txt = "(" ^ code ^ ")"; loc = Locations.location locs ~start:(start - 1) ~end_ }
     in
     let%map (expr : Ppxlib.expression) =
       (* borrowed from [ppx_string] *)
       match ocaml_expression code with
       | expr -> return expr
       | exception (Syntaxerr.Error error as exn) ->
         fail_but_we_know_location
           ~exn
           ~loc:(Syntaxerr.location_of_error error)
           "Failed to parse OCaml expression inside of HTML."
       | exception exn -> fail ~start ~end_ ~locs ~exn "Failed to Parse Expression"
     in
     let string_relative_location = { String_relative_location.start; end_ = end_ - 1 } in
     fun loc ->
       let expr = { Expr.expr; code; to_t; loc; string_relative_location } in
       { expr; interpolation_kind })
;;

let skip_while1 f = skip f *> skip_while f
let take_span_while1 ~locs f = with_loc ~locs (take_while1 f)

let parse_name ~allow_hyphens ~expected ~locs : string Loc.t Angstrom.t =
  take_span_while1 ~locs (function
    | '_' -> true
    | c -> Char.is_alphanum c || (allow_hyphens && Char.equal c '-'))
  <|> (peek_char
       >>= fun c ->
       let but =
         match c with
         | None -> ""
         | Some c ->
           let found =
             match Char.is_whitespace c with
             | true -> "whitespace. No whitespace is allowed here."
             | false -> [%string "'%{c#Char}'"]
           in
           [%string {|, but instead found %{found}|}]
       in
       fail ~locs [%string {|Expected a valid %{expected}%{but}.  |}])
;;

let parse_ocaml_name = parse_name ~allow_hyphens:false
let parse_attr_name = parse_name ~allow_hyphens:true

let parse_html_token ~locs : string Loc.t Angstrom.t =
  take_span_while1 ~locs (function
    | '_' | ':' | '-' | '.' -> true
    | c -> Char.is_alphanum c)
;;

let only_normal_interpolation_allowed ~locs ~interpolation_kind expr =
  match interpolation_kind with
  | Interpolation_kind.Normal -> return expr
  | (Option | List | String) as kind ->
    let normal_interpolation = "%{}" in
    fail
      ~locs
      [%string
        {|%{kind#Interpolation_kind} interpolation is not allowed here, only %{normal_interpolation} interpolation is allowed in this context.|}]
;;

let parse_quote_expr ~locs =
  let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
  only_normal_interpolation_allowed ~locs ~interpolation_kind (Model.Quote.Elt.Expr expr)
;;

let interpolation_case =
  choice
    [ "%{" => `Expression; "?{" => `Expression; "*{" => `Expression; "#{" => `Expression ]
;;

let parse_quote ~locs : Model.Quote.t Angstrom.t =
  let quoted =
    match%bind
      choice
        [ interpolation_case
        ; Char.is_whitespace >=> `Whitespace_literal
        ; return `Literal
        ]
    with
    | `Expression -> parse_quote_expr ~locs
    | `Whitespace_literal ->
      let%map str = with_loc ~locs (take_while1 Char.is_whitespace) in
      Model.Quote.Elt.Literal str
    | `Literal ->
      let%map str =
        with_loc
          ~locs
          (consumed
             (skip_many1
                (match%bind any_char with
                 | '"' -> fail ~locs "End of string"
                 | '\\' ->
                   (* escape... *)
                   let%map _ = any_char in
                   ()
                 | '%' ->
                   let%bind c = peek_char_fail in
                   if Char.( = ) c '{'
                   then fail ~locs "Expected code block"
                   else return ()
                 | c ->
                   if Char.is_whitespace c then fail ~locs "Whitespace" else return ()))
           >>| Scanf.unescaped)
      in
      Model.Quote.Elt.Literal str
  in
  with_loc
    ~locs
    (match%bind choice [ "\"" => `Quoted; return `Html_token ] with
     | `Quoted ->
       let%map () = string "\""
       and t = many quoted
       and () = string "\"" in
       t
     | `Html_token ->
       let%map t = parse_html_token ~locs in
       [ Model.Quote.Elt.Literal t ])
;;

let parse_attr_value_expr ~locs =
  let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
  only_normal_interpolation_allowed ~locs ~interpolation_kind (Model.Attr.Value.Expr expr)
;;

let parse_value ~locs : Model.Attr.Value.t Angstrom.t =
  match%bind choice [ interpolation_case; return `Literal ] with
  | `Expression -> parse_attr_value_expr ~locs
  | `Literal ->
    let%map str = parse_quote ~locs in
    Model.Attr.Value.Literal str
;;

let parse_attr_expr ~locs =
  let%map { expr; interpolation_kind } = parse_expr_common ~locs in
  Model.Attr.Expr { expr; interpolation_kind }
;;

let parse_attr ~locs : Model.Attr.t Angstrom.t =
  with_loc'
    ~locs
    (match%bind choice [ interpolation_case; return `Attr_name ] with
     | `Attr_name ->
       let%bind name = parse_attr_name ~expected:"name of attribute" ~locs in
       let%bind value =
         match%bind choice [ "=" => `Equal; return `No_equal ] with
         | `Equal ->
           let%map () = char '='
           and value = parse_value ~locs in
           Some value
         | `No_equal -> return None
       in
       let ret loc = Model.Attr.Attr { name; value; loc } in
       return ret
     | `Expression ->
       let%map expr = parse_attr_expr ~locs in
       fun _ -> expr)
;;

let parse_tag_expr ~locs =
  let%bind { expr; interpolation_kind } = parse_expr_common ~locs in
  only_normal_interpolation_allowed ~locs ~interpolation_kind (Model.Tag.Expr expr)
;;

let parse_tag ~locs : Model.Tag.t Angstrom.t =
  match%bind choice [ interpolation_case; ">" => `Fragment; return `Literal ] with
  | `Expression -> parse_tag_expr ~locs
  | `Fragment ->
    let%map { txt = (); loc } = with_loc ~locs (return ()) in
    Model.Tag.Fragment loc
  | `Literal ->
    let%map atom = parse_ocaml_name ~expected:"HTML tag" ~locs in
    Model.Tag.Literal atom
;;

let skip_opt_ws = skip_while Char.is_whitespace
let skip_req_ws = skip_while1 Char.is_whitespace

let skip_comment =
  string "<!--"
  *> scan_string
       Reversed_list.[]
       (fun prev c ->
         let prev = Reversed_list.(c :: prev) in
         match prev with
         | '>' :: '-' :: '-' :: _ -> None
         | _ -> Some prev)
  *> string ">"
  *> return ()
;;

let many_nodes ~parse_node =
  fix (fun (many_nodes : Node.t list t) ->
    match%bind
      choice
        [ interpolation_case
        ; "<!--" => `Comment
        ; "</" => `Closing
        ; "<" => `Element
        ; (match%bind peek_char with
           | None -> return `End
           | _ -> Angstrom.fail "If you see this error, it is a ppx_html bug.")
        ; return `Text
        ]
    with
    | `Closing | `End -> return []
    | `Expression | `Element | `Text ->
      let%bind current_node = parse_node
      and remaining = many_nodes in
      return (current_node :: remaining)
    | `Comment ->
      let%bind () = skip_comment in
      many_nodes)
  >>| List.filter ~f:(function
    | Node.Text { txt = ""; loc = _ } -> false
    | _ -> true)
;;

let fail_with_closing_tag ~(tag : Tag.t) ~locs =
  let element =
    match tag with
    | Literal { txt; _ } -> [%string {|element "%{txt}"|}]
    | Expr _ -> "element"
    | Fragment _ -> "fragment"
  in
  peek_char
  >>= function
  | None ->
    fail
      ~locs
      [%string
        {|Expected closing '>' to terminate %{element}, but found end of ppx_html expression|}]
  | Some c ->
    fail
      ~locs
      [%string {|Expected closing '>' to terminate %{element}, but found '%{c#Char}'|}]
;;

let many_attrs ~tag ~locs =
  fix (fun many_attrs ->
    match%bind choice [ ">" => `End; "/>" => `End; return `Must_have_whitespace ] with
    | `End -> return []
    | `Must_have_whitespace ->
      let%bind () = skip_req_ws <|> fail_with_closing_tag ~tag ~locs in
      (match%bind choice [ ">" => `End; "/>" => `End; return `Maybe_attr ] with
       | `End -> return []
       | `Maybe_attr ->
         let%map curr = parse_attr ~locs
         and rem = many_attrs in
         curr :: rem))
;;

let parse_element ~(parse_node : Node.t t) ~locs : Node.t Angstrom.t =
  with_loc'
    ~locs
    (let%bind { txt = tag, attrs, closed, open_string_relative_location; loc = open_loc } =
       with_loc
         ~locs
         (let%bind start = pos in
          let%bind () = char '<'
          and tag = parse_tag ~locs in
          let%map attrs = many_attrs ~locs ~tag
          and () = skip_opt_ws
          and closed = option (char '/' *> skip_opt_ws) >>| Option.is_some
          and end_ = pos
          and () = char '>' in
          tag, attrs, closed, { String_relative_location.start; end_ })
     in
     let error_message =
       lazy
         (let tag =
            match tag with
            | Model.Tag.Literal { txt; _ } -> txt
            | Model.Tag.Expr { expr; _ } -> Pprintast.string_of_expression expr
            | Model.Tag.Fragment _ -> "fragment (<></>)"
          in
          [%string {|No closing tag, but expected one for '%{tag}'|}])
     in
     let%map inner, close_string_relative_location =
       if closed
       then return (None, None)
       else (
         let%bind inner = many_nodes ~parse_node
         and close_start = pos in
         let%bind `Has_closing =
           match%bind peek_string 2 <|> fail ~locs (force error_message) with
           | "</" -> return `Has_closing
           | _ -> fail ~locs (force error_message)
         in
         let%bind () =
           let%map () = char '<'
           and () = char '/' in
           ()
         and () = skip_opt_ws
         and close_tag = option (parse_ocaml_name ~expected:"HTML tag" ~locs)
         and () = skip_opt_ws
         and close_end = pos
         and () = char '>' <|> fail_with_closing_tag ~tag ~locs in
         let open_tag =
           match tag with
           | Tag.Literal tag -> tag.txt
           | Expr _ -> ""
           | Fragment _ -> ""
         in
         let close_tag =
           match close_tag with
           | None -> ""
           | Some close_tag -> close_tag.txt
         in
         if String.( <> ) open_tag close_tag
         then
           fail
             ~start:close_start
             ~end_:close_end
             ~locs
             [%string "Expected closing tag </%{open_tag}>, got </%{close_tag}>"]
         else
           return
             ( Some inner
             , Some { String_relative_location.start = close_start; end_ = close_end } ))
     in
     fun loc ->
       Node.Element
         { tag
         ; attrs
         ; inner
         ; loc
         ; open_loc
         ; open_string_relative_location
         ; close_string_relative_location
         })
;;

let collapse_prefix_and_trailing_ws s =
  (* NOTE: This "collapses" whitespace so that it remains in-sync with the spec
     defined in: 

     https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace

     this does not "collapse" across adjacent whitespace between elements sadly.

     The "collapsing" whitespace part is optional. Another valid way of addressing
     this would be to solely leave the whitespace in, but this would have some runtime
     cost in addition to making the string harder to read during tests... This instead
     moves the cost to build time (at ppx expansion time).
  *)
  match String.for_all s ~f:Char.is_whitespace with
  | true -> " "
  | false ->
    let ws_prefix = String.take_while s ~f:Char.is_whitespace in
    let ws_suffix =
      String.take_while (String.rev s) ~f:Char.is_whitespace |> String.rev
    in
    let s =
      match ws_prefix with
      | "" -> s
      | _ -> " " ^ String.chop_prefix_exn ~prefix:ws_prefix s
    in
    let s =
      match ws_suffix with
      | "" -> s
      | _ -> String.chop_suffix_exn ~suffix:ws_suffix s ^ " "
    in
    s
;;

let string_until_interpolation_or_segment =
  let maybe_take s = s => `Take s in
  fix (fun string_until_interpolation_or_segment ->
    match%bind
      choice
        [ maybe_take "%%{"
        ; "%{" => `Finish
        ; "?{" => `Finish
        ; "*{" => `Finish
        ; "#{" => `Finish
        ; "<" => `Finish
        ; return `Consume
        ]
    with
    | `Finish -> return ()
    | `Take s -> string s *> string_until_interpolation_or_segment
    | `Consume ->
      (match%bind peek_char with
       | None -> return ()
       | Some c -> char c *> string_until_interpolation_or_segment))
;;

let parse_text ~locs : Node.t t =
  let%map str =
    with_loc
      ~locs
      (consumed string_until_interpolation_or_segment
       >>| (fun s ->
             if String.for_all s ~f:Char.is_whitespace && String.mem s '\n'
             then ""
             else collapse_prefix_and_trailing_ws s)
       >>| String.substr_replace_all ~pattern:"%%" ~with_:"%")
  in
  Node.Text str
;;

let parse_node_expr ~locs =
  let%map { expr; interpolation_kind } = parse_expr_common ~locs in
  Node.Expr { expr; interpolation_kind }
;;

let parse_node ~locs : Model.Node.t Angstrom.t =
  fix (fun parse_node ->
    match%bind choice [ interpolation_case; "<" => `Element; return `Text ] with
    | `Expression -> parse_node_expr ~locs
    | `Element -> parse_element ~parse_node ~locs
    | `Text -> parse_text ~locs)
;;

let parse ~locs =
  let%bind nodes = many_nodes ~parse_node:(parse_node ~locs) in
  match%bind peek_char with
  | None -> return nodes
  | Some _ ->
    (* NOTE: Here is unparsed input. Parsing will fail, but we still do things here to
       provide better error messages. I _think_ that this only happens with
       <div></div></div> though I am unsure it's __ONLY__ in that case hence
       the `Unknown case. *)
    (match%bind choice [ "</" => `Unopened_tag; return `Unknown ] with
     | `Unopened_tag -> fail ~locs "This closing tag was never opened."
     | `Unknown ->
       fail ~locs "Unparsed input. Please report this bug to ppx_html maintainers.")
;;

let of_string ~loc str =
  let locs = Locations.create loc str in
  let parse = parse ~locs in
  match
    Angstrom.parse_string
      ~consume:All
      (let%bind result = parse in
       let%bind () = end_of_input in
       return result)
      str
  with
  | Ok model -> model
  | Error message ->
    let error =
      let last =
        String.lsplit2 ~on:':' message |> Option.value_map ~f:snd ~default:message
      in
      try Sexp.of_string last |> Error.t_of_sexp with
      | _ -> Error.of_string last
    in
    Error.raise ~loc error
;;
