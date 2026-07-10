open Core
open Ppxlib
open Angstrom
open Angstrom.Let_syntax
open Model

module Locations : sig
  type t

  val create : Location.t -> string -> t
  val location : t -> start:int -> end_:int -> Location.t

  (** The 1-indexed source line number containing [offset]. *)
  val line_number_of_offset : t -> int -> int

  (** Render multiple annotated code snippets. Annotations on the same line are merged
      into a single snippet block with multiple caret rows (sorted by column). Annotations
      on different lines get separate snippet blocks, rendered in list order. *)
  val format_code_snippets
    :  t
    -> snippets:(int * int * string) list
    -> ?hint:string
    -> unit
    -> string
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

  (** [line_starts] returns the list of [(line_start_offset, line_number)] pairs for all
      lines in the text, sorted by offset. *)
  let line_starts { lines; _ } =
    Map.to_alist lines |> List.map ~f:(fun (offset, pos) -> offset, pos.pos_lnum)
  ;;

  (** [line_of_offset] returns [(line_start_offset, line_number)] given a byte offset in
      the text. *)
  let line_of_offset { lines; _ } offset =
    let () =
      if offset < 0
      then
        Location.raise_errorf
          ~loc:{ loc_start = [%here]; loc_end = [%here]; loc_ghost = true }
          "Somehow received negative offset while computing line number for error. This \
           is an error in [ppx_html], please contact the maintainers."
    in
    match Map.closest_key lines `Less_or_equal_to offset with
    | Some (line_start, pos) -> line_start, pos.pos_lnum
    | None -> 0, 1
  ;;

  let line_number_of_offset t offset = snd (line_of_offset t offset)

  (** [line_text] extracts the text of a single line starting at [substring_start_offset]
      until the next following newline character or EOF. *)
  let line_text { lines; text } substring_start_offset =
    let line_end =
      match Map.closest_key lines `Greater_than substring_start_offset with
      | Some (next_offset, _) ->
        (* [next_offset] points to the char after '\n', so the line ends at
           [next_offset - 1] (the '\n' itself). We exclude the '\n'. *)
        next_offset - 1
      | None -> String.length text
    in
    String.sub text ~pos:substring_start_offset ~len:(line_end - substring_start_offset)
  ;;

  let format_code_snippets t ~snippets ?hint () =
    List.iter snippets ~f:(fun (start, end_, _msg) ->
      if not (0 <= start && start <= end_ && end_ <= String.length t.text)
      then
        Location.raise_errorf
          ~loc:{ loc_start = [%here]; loc_end = [%here]; loc_ghost = true }
          "Snippet offsets [start=%d, end_=%d, length=%d] do not satisfy [0 <= start <= \
           end_ <= length]. This is an error in [ppx_html], please contact the \
           maintainers."
          start
          end_
          (String.length t.text));
    (* Group consecutive annotations that share a line. We walk the list in order, and
       whenever two adjacent annotations land on the same source line we merge them into
       the same group.

       Each group is rendered as a single snippet block. *)
    let groups =
      (* We can assume that if snippets are within the same line, then they are contiguous
         with other snippets on the same line *)
      List.fold snippets ~init:Int.Map.empty ~f:(fun acc (start, end_, msg) ->
        let _, line_num = line_of_offset t start in
        Map.update acc line_num ~f:(function
          | None -> Reversed_list.[ start, end_, msg ]
          | Some prev -> (start, end_, msg) :: prev))
    in
    let all_line_starts = line_starts t in
    let buf = Buffer.create (String.length t.text) in
    let open struct
      type t =
        { snippets : (int * int * string) list
        ; relevant_lines : (int * string) list
        }
    end in
    let groups =
      Map.mapi groups ~f:(fun ~key:error_line_num ~data:snippets ->
        let snippets = Reversed_list.rev snippets in
        let relevant_lines =
          let context_radius = 1 in
          let min_line = error_line_num - context_radius in
          let max_line = error_line_num + context_radius in
          List.filter all_line_starts ~f:(fun (_, lnum) ->
            lnum >= min_line && lnum <= max_line)
          |> List.map ~f:(fun (line_start_offset, lnum) ->
            let line = line_text t line_start_offset in
            lnum, line)
        in
        (* Sort snippets by column so the rightmost renders first. We print snippets
           rightmost to left to avoid snippets blocking subsequent snippets. *)
        let snippets =
          List.sort snippets ~compare:(Comparable.reverse [%compare__local: int * _ * _])
        in
        { relevant_lines; snippets })
    in
    (* Padding so line numbers are aligned properly *)
    let num_digits_in_highest_lnum =
      let max_lnum =
        Map.fold groups ~init:0 ~f:(fun ~key:_ ~data:{ relevant_lines; _ } acc ->
          List.fold relevant_lines ~init:acc ~f:(fun acc (lnum, _) -> Int.max acc lnum))
      in
      String.length (Int.to_string max_lnum)
    in
    let pad = String.make num_digits_in_highest_lnum ' ' in
    (* Yes in theory this is inefficient, but errors have a maximum of 2 groups *)
    Map.iteri groups ~f:(fun ~key:error_line_num ~data:{ snippets; relevant_lines } ->
      let ( (* Print a buffer line betwen the error message and the context text *) ) =
        Buffer.add_string buf [%string "%{pad} |\n"]
      in
      List.iter relevant_lines ~f:(fun (lnum, line) ->
        let ( (* Print out the line number + line text *) ) =
          let lnum_str = Int.to_string lnum in
          let padding_for_current_line =
            let num_spaces_for_current_line =
              num_digits_in_highest_lnum - String.length lnum_str
            in
            String.make num_spaces_for_current_line ' '
          in
          Buffer.add_string
            buf
            [%string "%{padding_for_current_line}%{lnum_str} | %{line}\n"]
        in
        if lnum = error_line_num
        then (
          let error_line_start, _ =
            List.find_exn all_line_starts ~f:(fun (_, lnum) -> lnum = error_line_num)
          in
          List.iter snippets ~f:(fun (caret_start, caret_end, msg) ->
            let col_start = caret_start - error_line_start in
            let col_end =
              if caret_end > caret_start
              then caret_end - error_line_start
              else col_start + 1
            in
            let line_len = String.length line in
            let col_end = Int.min col_end line_len in
            let col_start = Int.min col_start line_len in
            let num_carets = Int.max 1 (col_end - col_start) in
            let leading_spaces = String.make col_start ' ' in
            let carets = String.make num_carets '^' in
            Buffer.add_string buf [%string "%{pad} | %{leading_spaces}%{carets} %{msg}\n"])));
      (* End with another buffer line before the hint *)
      Buffer.add_string buf [%string "%{pad} |\n"]);
    (match hint with
     | None -> ()
     | Some hint -> Buffer.add_string buf [%string "%{pad} | Hint: %{hint}\n"]);
    Buffer.contents buf
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
      ; snippet : string option [@sexp.option]
      }
    [@@deriving sexp]
  end

  include T
  include Sexpable.To_stringable (T)

  let of_string str =
    try of_string str with
    | _ -> { location = None; message = str; snippet = None }
  ;;

  let raise ~loc { location; message; snippet } =
    let message =
      match snippet with
      | None -> message
      | Some snippet -> [%string "%{message}\n%{snippet}"]
    in
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
  fail (Error.to_string { location = Some loc; message; snippet = None })
;;

(** [fail] creates an error with the provided parameters.

    [snippets] get printed as a source location with a caret pointing to the exact span *)
let fail ?start ?end_ ~locs ?exn ?hint ?snippets message =
  let%bind pos in
  let start = Option.value start ~default:pos in
  let end_ = Option.value end_ ~default:pos in
  let snippets = Option.value snippets ~default:[ start, end_, "" ] in
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
  let formatted_snippet = Locations.format_code_snippets locs ~snippets ?hint () in
  fail
    (Error.to_string
       { location = Some location; message; snippet = Some formatted_snippet })
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
  let%bind start_pos = pos in
  let%bind code =
    scan_string "" (fun curr_string c ->
      (* NOTE: This code is semi-complex/ugly. What it does is adding support for being
         able to handle tricky edge cases like:

         [%html {|<div>%{Vdom.Node.text "}"}</div>|}]

         Solely reading a single character at a time results in the curly brace inside of
         the interpolated ocaml closing the entire segment despite it being inside of an
         escaped string.

         The way this code works is that it solely calls the OCaml tokenizer until it
         seems a string with an unclosed curly brace, at which point it stops scanning the
         string. *)
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
  | None | Some _ ->
    let error_pos = start_pos + String.length (String.rstrip code) in
    fail
      ~start:error_pos
      ~end_:error_pos
      ~locs
      ~snippets:[ error_pos, error_pos, "curly brace expected here" ]
      "Missing curly brace for interpolated OCaml"
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
     let interpolation_content_loc = Locations.location locs ~start ~end_ in
     let code, to_t =
       match Ocaml_parsing.rsplit_on_hash code with
       | None -> code, None
       | Some (code, to_t) ->
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
         code, Some to_t
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
       | exception exn ->
         fail
           ~start
           ~end_
           ~locs
           ~exn
           ~snippets:[ start, end_, "expression here" ]
           "Failed to parse OCaml expression"
     in
     let expr = { expr with pexp_loc = interpolation_content_loc } in
     let string_relative_location = { String_relative_location.start; end_ = end_ - 1 } in
     fun loc ->
       let expr =
         { Expr.expr; code; to_t; loc; string_relative_location; escape_kind = Escaped }
       in
       { expr; interpolation_kind })
;;

let skip_while1 f = skip f *> skip_while f
let take_span_while1 ~locs f = with_loc ~locs (take_while1 f)
let is_capitalized s = (not (String.is_empty s)) && Char.is_uppercase s.[0]

let parse_name ~allow_hyphens ~allow_dots ~expected ~locs : string Loc.t Angstrom.t =
  take_span_while1 ~locs (function
    | '_' -> true
    | c ->
      Char.is_alphanum c
      || Char.equal c '\''
      || (allow_hyphens && Char.equal c '-')
      (* NOTE: We want to allow dots for the "component" syntax:

         e.g. <Foo.f></> *)
      || (allow_dots && Char.equal c '.'))
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
let parse_attr_name = parse_name ~allow_hyphens:true ~allow_dots:false

let parse_html_token ~locs : string Loc.t Angstrom.t =
  take_span_while1 ~locs (function
    | '_' | ':' | '-' | '.' -> true
    | c -> Char.is_alphanum c)
;;

let only_normal_interpolation_allowed ~start ~end_ ~locs ~interpolation_kind expr =
  match interpolation_kind with
  | Interpolation_kind.Normal -> return expr
  | (Option | List | String) as kind ->
    let normal_interpolation = "%{}" in
    fail
      ~start
      ~end_
      ~locs
      ~snippets:[ start, end_, "invalid interpolation here" ]
      [%string
        {|%{kind#Interpolation_kind} interpolation is not allowed here, only %{normal_interpolation} interpolation is allowed in this context.|}]
;;

let parse_quote_expr ~locs =
  let%bind start = pos
  and { expr; interpolation_kind } = parse_expr_common ~locs
  and end_ = pos in
  only_normal_interpolation_allowed
    ~start
    ~end_
    ~locs
    ~interpolation_kind
    (Model.Quote.Elt.Expr expr)
;;

let interpolation_case =
  choice
    [ "%{" => `Expression; "?{" => `Expression; "*{" => `Expression; "#{" => `Expression ]
;;

let argument_case = choice [ "~" => `Argument `Tilde; "?" => `Argument `Question_mark ]

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
  let%bind start = pos
  and { expr; interpolation_kind } = parse_expr_common ~locs
  and end_ = pos in
  only_normal_interpolation_allowed
    ~start
    ~end_
    ~locs
    ~interpolation_kind
    (Model.Attr.Value.Expr expr)
;;

let parse_value ~locs : Model.Attr.Value.t Angstrom.t =
  match%bind choice [ interpolation_case; return `Literal ] with
  | `Expression -> parse_attr_value_expr ~locs
  | `Literal ->
    let%map str = parse_quote ~locs in
    Model.Attr.Value.Literal str
;;

let parse_tag_expr ~locs =
  let%bind start = pos
  and { expr; interpolation_kind } = parse_expr_common ~locs
  and end_ = pos in
  only_normal_interpolation_allowed
    ~start
    ~end_
    ~locs
    ~interpolation_kind
    (Model.Tag.Expr expr)
;;

let parse_tag ~locs : Model.Tag.t Angstrom.t =
  match%bind choice [ interpolation_case; ">" => `Fragment; return `Literal ] with
  | `Expression -> parse_tag_expr ~locs
  | `Fragment ->
    let%map { txt = (); loc } = with_loc ~locs (return ()) in
    Model.Tag.Fragment loc
  | `Literal ->
    let%map start = pos
    and atom = parse_ocaml_name ~expected:"HTML tag" ~locs ~allow_dots:true
    and end_ = pos in
    (match String.mem atom.txt '.' || is_capitalized atom.txt with
     | false -> Model.Tag.Literal (Literal atom)
     | true ->
       let name = { atom with txt = Ppxlib.Longident.parse atom.txt }
       and string_relative_location = { String_relative_location.start; end_ } in
       Model.Tag.Literal (Component { name; string_relative_location; code = atom }))
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

let collapse_prefix_and_trailing_ws s =
  (* NOTE: This "collapses" whitespace so that it remains in-sync with the spec defined
     in:

     https://developer.mozilla.org/en-US/docs/Web/API/Document_Object_Model/Whitespace

     this does not "collapse" across adjacent whitespace between elements sadly.

     The "collapsing" whitespace part is optional. Another valid way of addressing this
     would be to solely leave the whitespace in, but this would have some runtime cost in
     addition to making the string harder to read during tests... This instead moves the
     cost to build time (at ppx expansion time).
  *)
  match (not (String.is_empty s)) && String.for_all s ~f:Char.is_whitespace with
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

module Collapse_ws = struct
  (* [Collapse_ws] uses buffers to build strings because otherwise we'd have to either use
     a [char Reversed_list.t], which would have to be reversed and then concatenated, or a
     string, which takes [n] time to append each char to. *)
  type t =
    | Leading of (Buffer.t * bool)
    | Content of
        { content : Buffer.t
        ; trailing_whitespace : Buffer.t * bool
        }

  let join_whitespace
    ((ws, seen_newline) : Buffer.t * bool)
    (c : [ `Newline | `Whitespace of char ])
    =
    match seen_newline, c with
    | true, (`Newline | `Whitespace _) | false, `Newline ->
      (* If we've already seen a newline, we ignore all whitespace since this will either
         be trimmed or turned into a single space *)
      Buffer.reset ws;
      ws, true
    | false, `Whitespace c ->
      Buffer.add_char ws c;
      ws, false
  ;;

  let append_char t c =
    let c =
      match c with
      | '\n' | '\r' -> `Newline
      | (' ' | '\t') as c -> `Whitespace c
      | other -> `Content other
    in
    match t, c with
    | Leading leading, ((`Newline | `Whitespace _) as c) ->
      Leading (join_whitespace leading c)
    | Content ({ trailing_whitespace = ws; _ } as t), ((`Newline | `Whitespace _) as c) ->
      Content { t with trailing_whitespace = join_whitespace ws c }
    | Leading (buffer, true), `Content c ->
      let ( (* Reset the buffer since we've seen a newline which means we need to trim the
               leading whitespace *) )
        =
        Buffer.reset buffer
      in
      Buffer.add_char buffer c;
      Content { content = buffer; trailing_whitespace = Buffer.create 16, false }
    | Leading (buffer, false), `Content c ->
      let ( (* We will be reusing the leading whitespace buffer here since we haven't seen
               a newline, which means we want to preserve the whitespace. *) )
        =
        Buffer.add_char buffer c
      in
      Content { content = buffer; trailing_whitespace = Buffer.create 16, false }
    | Content { content; trailing_whitespace = ws, seen_trailing_newline; _ }, `Content c
      ->
      let ( (* Handle the whitespace first since it's the whitespace that has been
               trailing [content].

               In the middle of text content, whitespace containing a newline will be
               translated into a single space, while whitespace that does NOT contain a
               newline is preserved
               https://typescriptlang.org/play/?#code/DwEwlgbgfAUABHAhguAjFG4GN5xAgUxmAHpxog
            *) )
        =
        let () =
          match seen_trailing_newline with
          | true -> Buffer.add_char content ' '
          | false -> Buffer.add_buffer content ws
        in
        Buffer.reset ws
      in
      Buffer.add_char content c;
      Content { content; trailing_whitespace = ws, false }
  ;;

  let finalize = function
    | Leading (_, true) ->
      (* This is whitespace containing a newline. Trim all whitespace, which returns the
         empty string *)
      ""
    | Leading (ws, false) ->
      (* A single line of whitespace without newlines. We return the whitespace directly *)
      Buffer.contents ws
    | Content { content; trailing_whitespace = ws, seen_trailing_newline } ->
      let ( (* Update the contents buffer with the trailing whitespace buffer if no
               newline has been seen *) )
        =
        if not seen_trailing_newline then Buffer.add_buffer content ws;
        Buffer.reset ws
      in
      Buffer.contents content
  ;;
end

(** [collapse_ws] will:
    - In the middle of text content, whitespace containing a newline will be translated
      into a single space, whereas whitespace that does NOT contain a newline is preserved
    - For leading and trailing whitespace, whitespace containing a newline will be
      trimmed, whereas whitespace that does NOT contain a newline is preserved

    This is done to replicate what the TypeScript compiler does with whitespaces for jsx. *)
let collapse_ws s =
  let max_chars = String.length s in
  String.fold
    s
    ~init:(Collapse_ws.Leading (Buffer.create max_chars, false))
    ~f:Collapse_ws.append_char
  |> Collapse_ws.finalize
;;

module Processed_node = struct
  type t =
    | Expression of
        { expr : Expr.t
        ; interpolation_kind : Interpolation_kind.t
        }
    | Element of Element.t
    | Text of string loc

  let finalize ~(whitespace_behavior : [ `Jsx | `Collapse_leading_trailing ]) = function
    | Expression { expr; interpolation_kind } -> Node.Expr { expr; interpolation_kind }
    | Text { txt; loc } ->
      let previous = collapse_prefix_and_trailing_ws txt in
      let txt =
        match whitespace_behavior with
        | `Jsx -> collapse_ws txt
        | `Collapse_leading_trailing ->
          (* This is the old behavior, so we just pass [previous] *)
          previous
      in
      Node.Text { txt; loc }
    | Element element -> Node.Element element
  ;;
end

let many_nodes ~parse_node =
  fix (fun (many_nodes : Processed_node.t list t) ->
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
;;

let fail_with_closing_tag ~(tag : Tag.t) ~locs =
  let element =
    match tag with
    | Literal (Literal { txt; _ } | Component { name = _; code = { txt; _ }; _ }) ->
      [%string {|element "%{txt}"|}]
    | Expr _ -> "element"
    | Fragment _ -> "fragment"
  in
  peek_char
  >>= function
  | None ->
    let%bind pos in
    fail
      ~locs
      ~snippets:[ pos, pos, "missing '>' here" ]
      [%string
        {|Expected closing '>' to terminate %{element}, but found end of ppx_html expression|}]
  | Some c ->
    let hint =
      match c with
      | '-' ->
        (* Should have errored out before this, but adding for completeness *)
        "ppx_html doesn't allow dashes in HTML tags"
      | ':' -> "attributes use '=' instead of ':' when assigning to a value"
      | _ -> ""
    in
    let%bind pos in
    fail
      ~locs
      ~hint
      ~snippets:[ pos, pos, "expected '>' here" ]
      [%string {|Expected closing '>' to terminate %{element}, but found '%{c#Char}'|}]
;;

let parse_attr_expr ~locs =
  let%map { expr; interpolation_kind } = parse_expr_common ~locs in
  Model.Attr.Expr { expr; interpolation_kind }
;;

let rec parse_attr ~parse_node ~locs : Model.Attr.t Angstrom.t =
  with_loc'
    ~locs
    (match%bind choice [ interpolation_case; argument_case; return `Attr_name ] with
     | `Attr_name ->
       let%bind name = parse_attr_name ~expected:"attribute name" ~locs in
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
     | `Argument sigil ->
       let error_message =
         "Expected an OCaml interpolation (e.g. %{}) or HTML element (e.g. (<></>))"
       in
       let%map sigil =
         match sigil with
         | `Tilde -> string "~" *> return Model.Attr.Sigil.Tilde
         | `Question_mark -> string "?" *> return Model.Attr.Sigil.Question_mark
       and name = parse_attr_name ~expected:"argument name" ~locs
       and argument =
         match%bind
           choice [ ":" => `With_payload; "=" => `Equal_typo; return `Punned ]
         with
         | `Punned -> return None
         | `Equal_typo ->
           let%bind pos in
           fail
             ~locs
             ~snippets:[ pos, pos, "expected ':' here" ]
             ~hint:
               "named OCaml function arguments must be prefixed with '~' or '?' and use \
                ':' instead of '=' when providing a value"
             "Expected ':' to provide an argument value, but found '='"
         | `With_payload ->
           let%bind () = string ":" *> return () in
           (match%bind
              choice
                [ "%{" => `Interpolation
                ; "(" => `Element
                ; "<" => `Fail_element_missing_parens
                ; "{" => `Fail_missing_percent
                ; Char.is_alpha >=> `Fail_missing_interpolation
                ; return `Unknown
                ]
            with
            | `Unknown ->
              let%bind pos in
              fail ~locs ~snippets:[ pos, pos, "invalid argument here" ] error_message
            | `Interpolation ->
              let%bind start = pos
              and { expr; interpolation_kind } = parse_expr_common ~locs
              and end_ = pos in
              only_normal_interpolation_allowed
                ~start
                ~end_
                ~locs
                ~interpolation_kind
                (Some (Attr.Argument.Expr expr))
            | `Element ->
              let%bind () = char '('
              and () = skip_opt_ws
              and element = parse_element ~parse_node ~locs
              and () = skip_opt_ws
              and () = char ')' in
              return (Some (Attr.Argument.Element element))
            | `Fail_element_missing_parens ->
              let%bind pos in
              fail
                ~locs
                ~snippets:[ pos, pos, "invalid argument here" ]
                "ppx_html expressions must be wrapped in parentheses when used as \
                 arguments"
            | `Fail_missing_percent ->
              let percent_interpolation = "%{...}" in
              let%bind pos in
              fail
                ~locs
                ~snippets:[ pos, pos, "invalid argument here" ]
                ~hint:
                  [%string
                    "Did you mean to write %{percent_interpolation} instead of {...}? \
                     The percent sign is required."]
                error_message
            | `Fail_missing_interpolation ->
              let percent_interpolation = "%{...}" in
              let%bind pos in
              fail
                ~locs
                ~snippets:[ pos, pos, "invalid argument here" ]
                ~hint:
                  [%string
                    "Did you mean to write %{percent_interpolation}? Bare identifiers \
                     are not allowed here, wrap the expression in \
                     %{percent_interpolation}."]
                error_message)
       in
       let ret loc = Model.Attr.Argument { name; argument; loc; sigil } in
       ret
     | `Expression ->
       let%map expr = parse_attr_expr ~locs in
       fun _ -> expr)

and parse_element ~(parse_node : Processed_node.t t) ~locs : Element.t Angstrom.t =
  with_loc'
    ~locs
    (let%bind { txt = tag, attrs, closed, open_string_relative_location; loc = open_loc } =
       with_loc
         ~locs
         (let%bind start = pos in
          let%bind () = char '<'
          and tag = parse_tag ~locs in
          let%map attrs = many_attrs ~parse_node ~locs ~tag
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
            | Model.Tag.Literal
                ( Literal { txt; _ }
                | Component { name = _; code = { txt; _ }; string_relative_location = _ }
                  ) -> txt
            | Model.Tag.Expr { expr; _ } -> Pprintast.string_of_expression expr
            | Model.Tag.Fragment _ -> "fragment (<></>)"
          in
          [%string {|Expected closing tag for '%{tag}'|}])
     in
     let%map inner, closing_tag =
       if closed
       then return (None, None)
       else (
         let%bind inner = many_nodes ~parse_node
         and close_start = pos in
         let inner =
           let whitespace_behavior =
             match tag with
             | Literal (Literal { txt = "script" | "style"; _ }) ->
               (* For script and style tags, we want to preserve the whitespace so that we
                  don't mangle the javascript/css into something unusable. This is a
                  different behavior from how typescript handles script/style tags in JSX *)
               `Collapse_leading_trailing
             | _ -> `Jsx
           in
           List.map inner ~f:(Processed_node.finalize ~whitespace_behavior)
         in
         let fail_closing_tag ?extra_snippet error_message =
           let start = open_string_relative_location.start in
           (* We add 1 here because end_ is actually end_ - 1 for substring reasons *)
           let end_ = open_string_relative_location.end_ + 1 in
           let snippets = Option.to_list extra_snippet in
           fail
             ~start
             ~end_
             ~snippets:((start, end_, "opening tag found here") :: snippets)
             ~locs
             error_message
         in
         let%bind error_pos = pos in
         let%bind `Has_closing =
           let fail_has_closing =
             let error_message = [%string "%{force error_message}, found EOF instead"] in
             let extra_snippet =
               let opening_tag_line =
                 Locations.line_number_of_offset locs open_string_relative_location.start
               in
               let eof_line = Locations.line_number_of_offset locs error_pos in
               (* If the file immediately ends on the same line, we don't print the second
                  caret to avoid clutter *)
               if opening_tag_line = eof_line
               then None
               else Some (error_pos, error_pos, "closing tag expected here")
             in
             fail_closing_tag ?extra_snippet error_message
           in
           match%bind peek_string 2 <|> fail_has_closing with
           | "</" -> return `Has_closing
           | _ -> fail_has_closing
         in
         let%bind () =
           let%map () = char '<'
           and () = char '/' in
           ()
         and () = skip_opt_ws
         and close_tag =
           option (parse_ocaml_name ~expected:"HTML tag" ~locs ~allow_dots:true)
         and () = skip_opt_ws
         and close_end = pos
         and () = char '>' <|> fail_with_closing_tag ~tag ~locs in
         let open struct
           (* NOTE: This is a bit complex. There are roughly three scenarios:

              - "Normal" tags, (e.g. <div></div>): The closing tag _must_ match the
                opening tag.

              - "Interpolated" tags/Fragments (e.g. <></> or <%[{F}]></>): The closing tag
                _must_ be empty.

              - "Component" tags (e.g. <Foo.f></> or <Foo.f></Foo.f>): The closing tag may
                be empty or must match the opening tag.
           *)
           type expected_closing_tag =
             | Empty
             | Empty_or_string of string
             | String of string
         end in
         let expected_closing_tag =
           match tag with
           | Tag.Literal (Literal tag) -> String tag.txt
           | Literal (Component { name; string_relative_location = _; code = _ }) ->
             Empty_or_string (Ppxlib.Longident.name name.txt)
           | Expr _ -> Empty
           | Fragment _ -> Empty
         in
         let ok =
           lazy
             (let loc =
                { String_relative_location.start = close_start; end_ = close_end }
              in
              let is_fragment_like =
                match close_tag with
                | Some { txt = ""; loc = _ } | None -> true
                | _ -> false
              in
              return (Some inner, Some { Closing_tag.loc; is_fragment_like }))
         in
         let fail message =
           fail_closing_tag (* We add one cause close_end is actually the end - 1 *)
             ~extra_snippet:(close_start, close_end + 1, "invalid closing tag here")
             message
         in
         match expected_closing_tag with
         | Empty ->
           (match close_tag with
            | None -> force ok
            | Some wrong ->
              fail
                [%string "Expected an empty (</>) closing tag, but got </%{wrong.txt}>."])
         | String expected ->
           (match close_tag with
            | None ->
              fail
                [%string "Expected </%{expected}>, but got an empty closing tag (</>)."]
            | Some { txt = got; loc = _ } ->
              if String.equal got expected
              then force ok
              else
                fail [%string "Expected closing tag </%{expected}>, but got </%{got}>."])
         | Empty_or_string expected ->
           (match close_tag with
            | None -> force ok
            | Some { txt = got; loc = _ } ->
              if String.equal got expected
              then force ok
              else
                fail [%string "Expected closing tag </%{expected}>, but got </%{got}>."]))
     in
     fun loc ->
       { Element.tag
       ; attrs
       ; inner
       ; loc
       ; open_loc
       ; open_string_relative_location
       ; closing_tag
       })

and many_attrs ~parse_node ~tag ~locs =
  fix (fun many_attrs ->
    match%bind choice [ ">" => `End; "/>" => `End; return `Must_have_whitespace ] with
    | `End -> return []
    | `Must_have_whitespace ->
      let%bind () = skip_req_ws <|> fail_with_closing_tag ~tag ~locs in
      (match%bind choice [ ">" => `End; "/>" => `End; return `Maybe_attr ] with
       | `End -> return []
       | `Maybe_attr ->
         let%map curr = parse_attr ~parse_node ~locs
         and rem = many_attrs in
         curr :: rem))
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

let parse_text ~locs : string loc t =
  with_loc
    ~locs
    (consumed string_until_interpolation_or_segment
     >>| fun s ->
     if String.for_all s ~f:Char.is_whitespace && String.mem s '\n'
     then ""
     else String.substr_replace_all ~pattern:"%%" ~with_:"%" s)
;;

let parse_node_expr ~locs =
  let%map { expr; interpolation_kind } = parse_expr_common ~locs in
  Processed_node.Expression { expr; interpolation_kind }
;;

let parse_node ~locs : Processed_node.t Angstrom.t =
  fix (fun parse_node ->
    match%bind choice [ interpolation_case; "<" => `Element; return `Text ] with
    | `Expression -> parse_node_expr ~locs
    | `Element ->
      let%map.Angstrom element = parse_element ~parse_node ~locs in
      Processed_node.Element element
    | `Text ->
      let%map.Angstrom text = parse_text ~locs in
      Processed_node.Text text)
;;

let filter_empty_text_nodes nodes =
  List.filter nodes ~f:(function
    | Node.Text { txt = ""; _ } -> false
    | _ -> true)
;;

let map_nodes =
  object
    inherit Model.Traverse.map as super

    method! element ({ inner; _ } as element) =
      let inner =
        let%map.Option inner in
        filter_empty_text_nodes inner
      in
      let element = { element with inner } in
      super#element element
  end
;;

let map_filter_empty_text_nodes nodes =
  filter_empty_text_nodes nodes |> List.map ~f:map_nodes#node
;;

let parse ~filter_empty_text_nodes ~locs =
  let%bind nodes = many_nodes ~parse_node:(parse_node ~locs) in
  let nodes =
    (* Top-level nodes should all be treated as [jsx] as by definition we cannot be inside
       a [script] tag since this is the top level *)
    List.map nodes ~f:(Processed_node.finalize ~whitespace_behavior:`Jsx)
  in
  let nodes =
    (* We're filtering at the top level so that we can retrieve the empty text nodes if
       necessary. This is useful for tree smashes, as removing the empty text nodes
       actually removes some information about the code that is crucial to generating
       correct treesmashes that deal with whitespace *)
    match filter_empty_text_nodes with
    | true -> map_filter_empty_text_nodes nodes
    | false -> nodes
  in
  match%bind peek_char with
  | None -> return nodes
  | Some _ ->
    (* NOTE: Here is unparsed input. Parsing will fail, but we still do things here to
       provide better error messages. I _think_ that this only happens with
       <div></div></div> though I am unsure it's __ONLY__ in that case hence the `Unknown
       case. *)
    (match%bind choice [ "</" => `Unopened_tag; return `Unknown ] with
     | `Unopened_tag ->
       let%bind pos in
       fail
         ~snippets:[ pos, pos, "closing tag here" ]
         ~locs
         "This closing tag was never opened."
     | `Unknown ->
       fail ~locs "Unparsed input. Please report this bug to ppx_html maintainers.")
;;

let of_string ?(filter_empty_text_nodes = true) ~loc str =
  let locs = Locations.create loc str in
  let parse = parse ~filter_empty_text_nodes ~locs in
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

module Private = struct
  let collapse_prefix_and_trailing_ws = collapse_prefix_and_trailing_ws
  let collapse_ws = collapse_ws
end
