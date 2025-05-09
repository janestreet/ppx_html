open! Core
open Ppxlib
open Ppx_html_syntax

let features_that_are_waiting_an_apply_style_roll_allowed = ref false

let set_features_that_are_waiting_an_apply_style_roll_allowed () =
  features_that_are_waiting_an_apply_style_roll_allowed := true
;;

let register_feature_flag () =
  (* NOTE: This is dead code should we want to do a similar feature-flag release lock in
     the future. *)
  Driver.add_arg
    "-features-that-are-waiting-an-apply-style-roll-allowed"
    (Unit set_features_that_are_waiting_an_apply_style_roll_allowed)
    ~doc:
      {|<unit> some features might be styled weirdly until they are picked up by apply
style e.g. <Component.f></>. You can use these features before they are styled properly
by apply style by sending this flag. It is not totally recommended as apply style
might do wrong things.|}
;;

let () = register_feature_flag ()

let loc_ghoster =
  object
    inherit Ast_traverse.map as super
    method! location location = super#location { location with loc_ghost = true }
  end
;;

let experimental_feature_checker ~loc:_ =
  object
    inherit Model.Traverse.map as super

    method! literal literal =
      match literal with
      | Literal _ as literal -> super#literal literal
      | Component { name = { loc; _ }; string_relative_location = _; code = _ } ->
        Location.raise_errorf
          ~loc
          "Error! This syntax is new and apply style does not understand it yet so it \
           may do wrong things! You can pass \
           '-features-that-are-waiting-an-apply-style-roll-allowed' to silence this \
           warning. "
  end
;;

let extension ~name ~runtime_kind ~experimental_features_allowed =
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
      let experimental_features_allowed =
        experimental_features_allowed
        || !features_that_are_waiting_an_apply_style_roll_allowed
      in
      if not experimental_features_allowed
      then
        List.iter model ~f:(fun node ->
          (ignore : Model.Node.t -> unit) ((experimental_feature_checker ~loc)#node node));
      Model_code_gen.code ~loc:outer_loc ~html_syntax_module ~runtime_kind model
      |> loc_ghoster#expression)
;;
