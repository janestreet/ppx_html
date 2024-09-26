open! Core
open Virtual_dom.Vdom.Html_syntax

(* NOTE: This file just contains some "smoke" tests that actually make the calls/run the
   virtual dom functions. These tests are not super exhaustive. The exhaustive tests
   live on the other directories, these test just test the happy paths/running of the ppx, assert
   that the ppx calls the right "runtime" functions (e.g. if the ppx_html runtime library where to
   do wrong things, we would know it here). *)

module View = struct
  let hbox ?gap ?attrs children =
    let maybe_gap = Option.map gap ~f:(fun gap -> {%css|gap: %{gap#Css_gen.Length};|}) in
    let attrs = Option.value ~default:[] attrs in
    {%html|
        <div ?{maybe_gap} *{attrs} style="display: flex; flex-direction: row">
          *{children}
        </div>
      |}
  ;;
end

let test vdom =
  print_endline
    (Virtual_dom_test_helpers.Node_helpers.to_string_html
       (Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn vdom))
;;

let%expect_test "basic" =
  test {%html|<div></div>|};
  [%expect {| <div> </div> |}];
  test
    {%html|
        <div>
          <h1>Hello World</h1>
          <ul>
            <li>Capy1</li>
            <li>Capy2</li>
            <li>Capy3</li>
            <li>Capy4</li>
            <li>Capy5</li>
            <li>Capy6</li>
          </ul>
        </div>
      |};
  [%expect
    {|
    <div>
      <h1> Hello World </h1>
      <ul>
        <li> Capy1 </li>
        <li> Capy2 </li>
        <li> Capy3 </li>
        <li> Capy4 </li>
        <li> Capy5 </li>
        <li> Capy6 </li>
      </ul>
    </div>
    |}]
;;

let%expect_test "Tag interpolation" =
  test
    {%html|
        <%{View.hbox ~gap:(`Rem 1.0)}>
          <h1>Hello World</h1>
        </>
      |};
  [%expect
    {|
    <div class="ppx_css_anonymous_class_hash_replaced_in_test ppx_css_anonymous_class_hash_replaced_in_test"
         custom-css-vars=((--ppx_css_anonymous_var_1_hash_replaced_in_test 1.00rem))>
      <h1> Hello World </h1>
    </div>
    |}]
;;

let%expect_test "Node interpolation" =
  let greeting = {%html|<h1>Hello world!</h1>|} in
  (* NOTE: virtual_dom_test_helpers seems to not respect HTML's whitespace rules, so the
     expect test output of this should not be the ground-truth for ppx_html. *)
  test {%html|<div>%{greeting}</div>|};
  [%expect
    {|
    <div>
      <h1> Hello world! </h1>
    </div>
    |}];
  test {%html|<div>%{"Hello World!"#String}</div>|};
  [%expect {| <div> Hello World! </div> |}]
;;

let%expect_test "Attribute interpolation" =
  let url = "URL" in
  let tomato = {%css|background-color: tomato;|} in
  test {%html|<div src=%{url} %{tomato}></div>|};
  [%expect
    {| <div src="URL" class="ppx_css_anonymous_class_hash_replaced_in_test"> </div> |}]
;;

let%expect_test "Node list interpolation" =
  let capys = List.init 10 ~f:(fun i -> {%html|<li>Capy%{i#Int}</li>|}) in
  test
    {%html|
        <div>
          <h1>Capybaras</h1>
          <ul>
            *{capys}
          </ul>
        </div>
      |};
  [%expect
    {|
    <div>
      <h1> Capybaras </h1>
      <ul>
        <li> Capy 0 </li>
        <li> Capy 1 </li>
        <li> Capy 2 </li>
        <li> Capy 3 </li>
        <li> Capy 4 </li>
        <li> Capy 5 </li>
        <li> Capy 6 </li>
        <li> Capy 7 </li>
        <li> Capy 8 </li>
        <li> Capy 9 </li>
      </ul>
    </div>
    |}]
;;

let%expect_test "Node list interpolation (with module)" =
  let capys = List.init 10 ~f:(fun i -> {%string|Capy%{i#Int}|}) in
  test
    {%html|
        <div>
          <h1>Capybaras</h1>
          <ul>
            *{capys#String}
          </ul>
        </div>
      |};
  [%expect
    {|
    <div>
      <h1> Capybaras </h1>
      <ul> Capy0 Capy1 Capy2 Capy3 Capy4 Capy5 Capy6 Capy7 Capy8 Capy9 </ul>
    </div>
    |}]
;;

let%expect_test "Node option interpolation" =
  let capy1 = None in
  let capy2 = Some {%html|<div>Capy</div>|} in
  test {%html|<div>?{capy1} ?{capy2}</div>|};
  [%expect
    {|
    <div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>

      <div> Capy </div>
    </div>
    |}]
;;

let%expect_test "Node option interpolation (with module)" =
  let capy1 = None in
  let capy2 = Some "capy" in
  test {%html|<div>?{capy1#String} ?{capy2#String}</div>|};
  [%expect
    {|
    <div>
      <Vdom.Node.none-widget> </Vdom.Node.none-widget>

      capy
    </div>
    |}]
;;

let%expect_test "Attr list interpolation" =
  let attrs =
    List.init 10 ~f:(fun i ->
      Virtual_dom.Vdom.Attr.create [%string "custom-attr-%{i#Int}"] "")
  in
  test {%html|<div *{attrs}></div>|};
  [%expect
    {|
    <div custom-attr-0=""
         custom-attr-1=""
         custom-attr-2=""
         custom-attr-3=""
         custom-attr-4=""
         custom-attr-5=""
         custom-attr-6=""
         custom-attr-7=""
         custom-attr-8=""
         custom-attr-9=""> </div>
    |}]
;;

let%expect_test "Attr list interpolation (with module)" =
  let module Foo = struct
    let to_attr x = Virtual_dom.Vdom.Attr.create [%string "custom-attr-%{x#Int}"] ""
  end
  in
  let attrs = List.init 10 ~f:(fun i -> i) in
  test {%html|<div *{attrs#Foo}></div>|};
  [%expect
    {|
    <div custom-attr-0=""
         custom-attr-1=""
         custom-attr-2=""
         custom-attr-3=""
         custom-attr-4=""
         custom-attr-5=""
         custom-attr-6=""
         custom-attr-7=""
         custom-attr-8=""
         custom-attr-9=""> </div>
    |}]
;;

let%expect_test "Attr option interpolation" =
  let attr1 = None in
  let attr2 = Some {%css|background-color: tomato;|} in
  test {%html|<div ?{attr1} ?{attr2}></div>|};
  [%expect {| <div class="ppx_css_anonymous_class_hash_replaced_in_test"> </div> |}]
;;

let%expect_test "Attr option interpolation (with module)" =
  let module Foo = struct
    let to_attr x = Virtual_dom.Vdom.Attr.create [%string "custom-attr-%{x}"] ""
  end
  in
  let attr1 = None in
  let attr2 = Some "an_attr" in
  test {%html|<div ?{attr1#Foo} ?{attr2#Foo}></div>|};
  [%expect {| <div custom-attr-an_attr=""> </div> |}]
;;

let%expect_test "string interpolation" =
  let capy = "Capybara"
  and animal = "rodent" in
  test {%html|<div>#{capy}'s are the world's largest #{animal}</div>|};
  [%expect {| <div> Capybara 's are the world's largest  rodent </div> |}]
;;
