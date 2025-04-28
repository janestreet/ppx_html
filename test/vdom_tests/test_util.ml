open! Core

let test vdom =
  print_endline
    (Virtual_dom_test_helpers.Node_helpers.to_string_html
       (Virtual_dom_test_helpers.Node_helpers.unsafe_convert_exn vdom))
;;
