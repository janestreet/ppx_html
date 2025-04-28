open! Core
open! Test_utils

let%expect_test "Basic use of [F.f]" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.f>Hihi</>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.f>Hihi</Foo.f>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.Bar.f>Hihi</>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    test {|<Foo.Bar.f>Hihi</Foo.Bar.f>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}]
;;

let%expect_test "Basic use of [F.f] (no children)" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.f />|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.Bar.f />|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}]
;;

let%expect_test "With a tilde!" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.foo'></>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.foo'></Foo.foo'>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.foo' />|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}]
;;

let%expect_test "Mismatched closing tag" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.f></Bar.f>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}]
;;

let%expect_test "OCaml arguments" =
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.f ~foo:%{EXPR}></>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () -> test {|<Foo.f ~punned></>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|<Foo.f
          ~foo:%{EXPR}
          ~bar:%{EXPR2}
          attr1=%{EXPR}
          bar=but-as-an-attr
          ~bam
        ></>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}];
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|<%{component}
        ~foo:%{EXPR}
        ~bar:%{EXPR2}
        attr1=%{EXPR}
        bar=but-as-an-attr
        ~bam
      ></>|});
  [%expect {| ("Expected a valid name of attribute, but instead found '~'.  ") |}]
;;

let%expect_test "Double attr" =
  Expect_test_helpers_core.require_does_raise (fun () ->
    test
      {|<Foo.f
          ~foo:%{EXPR}
          ~bar:%{EXPR2}
          ~attr:%{[]}
          attr1=%{EXPR}
          bar=but-as-an-attr
          ~bam
        ></>|});
  [%expect {| ("Expected closing '>' to terminate element \"Foo\", but found '.'") |}]
;;
