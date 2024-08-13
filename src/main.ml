open! Core
open Ppxlib
open Ppx_html_syntax

let features_that_are_waiting_an_apply_style_roll_allowed = ref false

let set_features_that_are_waiting_an_apply_style_roll_allowed () =
  features_that_are_waiting_an_apply_style_roll_allowed := true
;;

let _register_feature_flag () =
  (* NOTE: This is dead code left around should we want to use it again in the
     future. *)
  Driver.add_arg
    "-features-that-are-waiting-an-apply-style-roll-allowed"
    (Unit set_features_that_are_waiting_an_apply_style_roll_allowed)
    ~doc:
      {|<unit> some features might be styled weirdly until they are picked up by apply
style e.g. <></>. You can use these features before they are styled properly
by apply style by sending this flag. It is not totally recommended as apply style
might do wrong things.|}
;;

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let experimental_feature_checker =
  (* NOTE: This is unused code that is left around should we want to do a similar thing
     in the future. *)
  object
    inherit Model.Traverse.map
  end
;;

let declare name =
  Extension.declare_with_path_arg
    name
    Extension.Context.expression
    Ast_pattern.(
      pstr (pstr_eval (pexp_constant (pconst_string __' __ (some __))) nil ^:: nil))
    (fun ~loc:outer_loc
      ~path:_
      ~arg:html_syntax_module
      { loc = _; txt = string }
      string_loc
      _delimiter ->
      let loc = string_loc in
      let model = Model_parser.of_string ~loc string in
      if false
         (* NOTE: This "checker mechanism" is left commented out/never runs should we ever
         want to use it again in the future. *)
      then
        List.iter model ~f:(fun node ->
          (ignore : Model.Node.t -> unit) (experimental_feature_checker#node node));
      Model_code_gen.code ~loc:outer_loc ~html_syntax_module model
      |> loc_ghoster#expression)
;;

let () =
  Ppxlib.Driver.register_transformation "ppx_html" ~extensions:[ declare "ppx_html.html" ]
;;
