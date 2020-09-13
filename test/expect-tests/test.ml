



let brace context s =
  match s with
  | "a" -> "AA" ^ context
  | "bb" -> "BB" ^ context
  | "b" -> "b"
  | x -> "<{" ^ x ^ "}>"
let paren context s =
  match s with
  | "c" -> "CC" ^ context
  | "dd" -> "DD" ^ context
  | "d" -> "d"
  | x -> "<(" ^ x ^ ")>"
let bracket context s =
  match s with
  | "e" -> "EE" ^ context
  | "ff" -> "FF" ^ context
  | "f" -> "f"
  | x -> "<[" ^ x ^ "]>"
let var context s =
  match s with
  | "g" -> "GG" ^ context
  | "hh" -> "HH" ^ context
  | "h" -> "h"
  | x -> "<[" ^ x ^ "]>"
let context = "!"

let cases =
  List.map (fun sym ->
      List.map (fun brace ->
          List.map (fun paren ->
              List.map (fun bracket ->
                  List.map (fun var ->
                      let ident =
                        Printf.sprintf
                          "sym=%b brace=%b paren=%b bracket=%b var=%b"
                          sym
                          (brace != None)
                          (paren != None)
                          (bracket != None)
                          (var != None)
                      in
                      let subst =
                        Ez_subst.string ~sym ?var ?brace ?paren ?bracket context
                      in
                      (ident, subst)
                    ) [ None ; Some var ]
                ) [ None ; Some bracket ]
            ) [ None ; Some paren ]
        ) [ None ; Some brace ]
    ) [false;true]
  |> List.flatten
  |> List.flatten
  |> List.flatten
  |> List.flatten

let sub tests  =
  List.iter (fun s->
      List.iter (fun (ident, subst) ->
          match subst s with
          | exception exn ->
              Printf.printf "%s: subst(%S) raised %s\n%!"
                ident s (Printexc.to_string exn)
          | res ->
              Printf.printf "%s: subst(%S)=%S\n%!"
                ident s res
        ) cases
    ) tests

(* if you add a test:
   1/ run the tests with
      $ drom test
    or
      $ dune build @runtest
   2/ check that the results are as expected
   3/ promote the new version:
      $ dune promote
*)

let%expect_test "substitutions" =
  sub
    [

      (* braces *)
      "${a}$" ;
      "${bb}$" ;
      "${a}$${bb}$${a}$" ;
      "${b${b}$}$${a}$${bb}$";
      "${a";
      "${bb}";
      "${bb}{}$" ;
      "${";

      (* braces and escape *)
      "\\${a}}$" ;
      "$\\{a}}$" ;
      "${\\a}}$" ;
      "${a\\}}$" ;
      "${a}\\}$" ;
      "${a}}\\$" ;

    ];
  [%expect {|
    sym=false brace=false paren=false bracket=false var=false: subst("${a}$")="${a}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${a}$")="${a}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${a}$")="${a}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${a}$")="${a}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${a}$")="${a}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${a}$")="${a}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${a}$")="${a}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${a}$")="${a}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${a}$")="AA!$"
    sym=false brace=true paren=false bracket=false var=true: subst("${a}$")="AA!$"
    sym=false brace=true paren=false bracket=true var=false: subst("${a}$")="AA!$"
    sym=false brace=true paren=false bracket=true var=true: subst("${a}$")="AA!$"
    sym=false brace=true paren=true bracket=false var=false: subst("${a}$")="AA!$"
    sym=false brace=true paren=true bracket=false var=true: subst("${a}$")="AA!$"
    sym=false brace=true paren=true bracket=true var=false: subst("${a}$")="AA!$"
    sym=false brace=true paren=true bracket=true var=true: subst("${a}$")="AA!$"
    sym=true brace=false paren=false bracket=false var=false: subst("${a}$")="${a}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${a}$")="${a}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${a}$")="${a}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${a}$")="${a}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${a}$")="${a}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${a}$")="${a}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${a}$")="${a}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${a}$")="${a}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${a}$")="AA!"
    sym=true brace=true paren=false bracket=false var=true: subst("${a}$")="AA!"
    sym=true brace=true paren=false bracket=true var=false: subst("${a}$")="AA!"
    sym=true brace=true paren=false bracket=true var=true: subst("${a}$")="AA!"
    sym=true brace=true paren=true bracket=false var=false: subst("${a}$")="AA!"
    sym=true brace=true paren=true bracket=false var=true: subst("${a}$")="AA!"
    sym=true brace=true paren=true bracket=true var=false: subst("${a}$")="AA!"
    sym=true brace=true paren=true bracket=true var=true: subst("${a}$")="AA!"
    sym=false brace=false paren=false bracket=false var=false: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${bb}$")="${bb}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${bb}$")="${bb}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${bb}$")="BB!$"
    sym=false brace=true paren=false bracket=false var=true: subst("${bb}$")="BB!$"
    sym=false brace=true paren=false bracket=true var=false: subst("${bb}$")="BB!$"
    sym=false brace=true paren=false bracket=true var=true: subst("${bb}$")="BB!$"
    sym=false brace=true paren=true bracket=false var=false: subst("${bb}$")="BB!$"
    sym=false brace=true paren=true bracket=false var=true: subst("${bb}$")="BB!$"
    sym=false brace=true paren=true bracket=true var=false: subst("${bb}$")="BB!$"
    sym=false brace=true paren=true bracket=true var=true: subst("${bb}$")="BB!$"
    sym=true brace=false paren=false bracket=false var=false: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${bb}$")="${bb}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${bb}$")="${bb}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${bb}$")="BB!"
    sym=true brace=true paren=false bracket=false var=true: subst("${bb}$")="BB!"
    sym=true brace=true paren=false bracket=true var=false: subst("${bb}$")="BB!"
    sym=true brace=true paren=false bracket=true var=true: subst("${bb}$")="BB!"
    sym=true brace=true paren=true bracket=false var=false: subst("${bb}$")="BB!"
    sym=true brace=true paren=true bracket=false var=true: subst("${bb}$")="BB!"
    sym=true brace=true paren=true bracket=true var=false: subst("${bb}$")="BB!"
    sym=true brace=true paren=true bracket=true var=true: subst("${bb}$")="BB!"
    sym=false brace=false paren=false bracket=false var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=false bracket=false var=true: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=false bracket=true var=false: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=false bracket=true var=true: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=true bracket=false var=false: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=true bracket=false var=true: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=true bracket=true var=false: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=false brace=true paren=true bracket=true var=true: subst("${a}$${bb}$${a}$")="AA!$BB!$AA!$"
    sym=true brace=false paren=false bracket=false var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${a}$${bb}$${a}$")="${a}$${bb}$${a}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=false bracket=false var=true: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=false bracket=true var=false: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=false bracket=true var=true: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=true bracket=false var=false: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=true bracket=false var=true: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=true bracket=true var=false: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=true brace=true paren=true bracket=true var=true: subst("${a}$${bb}$${a}$")="AA!BB!AA!"
    sym=false brace=false paren=false bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=false bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=false bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=false bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=true bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=true bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=true bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=false brace=true paren=true bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="<{bb$}>$AA!$BB!$"
    sym=true brace=false paren=false bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="${b${b}$}$${a}$${bb}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=false bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=false bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=false bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=true bracket=false var=false: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=true bracket=false var=true: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=true bracket=true var=false: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=true brace=true paren=true bracket=true var=true: subst("${b${b}$}$${a}$${bb}$")="BB!AA!BB!"
    sym=false brace=false paren=false bracket=false var=false: subst("${a")="${a"
    sym=false brace=false paren=false bracket=false var=true: subst("${a")="${a"
    sym=false brace=false paren=false bracket=true var=false: subst("${a")="${a"
    sym=false brace=false paren=false bracket=true var=true: subst("${a")="${a"
    sym=false brace=false paren=true bracket=false var=false: subst("${a")="${a"
    sym=false brace=false paren=true bracket=false var=true: subst("${a")="${a"
    sym=false brace=false paren=true bracket=true var=false: subst("${a")="${a"
    sym=false brace=false paren=true bracket=true var=true: subst("${a")="${a"
    sym=false brace=true paren=false bracket=false var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=false bracket=false var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=false bracket=true var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=false bracket=true var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=true bracket=false var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=true bracket=false var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=true bracket=true var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=true paren=true bracket=true var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=false paren=false bracket=false var=false: subst("${a")="${a"
    sym=true brace=false paren=false bracket=false var=true: subst("${a")="${a"
    sym=true brace=false paren=false bracket=true var=false: subst("${a")="${a"
    sym=true brace=false paren=false bracket=true var=true: subst("${a")="${a"
    sym=true brace=false paren=true bracket=false var=false: subst("${a")="${a"
    sym=true brace=false paren=true bracket=false var=true: subst("${a")="${a"
    sym=true brace=false paren=true bracket=true var=false: subst("${a")="${a"
    sym=true brace=false paren=true bracket=true var=true: subst("${a")="${a"
    sym=true brace=true paren=false bracket=false var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=false bracket=false var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=false bracket=true var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=false bracket=true var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=true bracket=false var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=true bracket=false var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=true bracket=true var=false: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=true brace=true paren=true bracket=true var=true: subst("${a") raised Ez_subst.UnclosedExpression("a")
    sym=false brace=false paren=false bracket=false var=false: subst("${bb}")="${bb}"
    sym=false brace=false paren=false bracket=false var=true: subst("${bb}")="${bb}"
    sym=false brace=false paren=false bracket=true var=false: subst("${bb}")="${bb}"
    sym=false brace=false paren=false bracket=true var=true: subst("${bb}")="${bb}"
    sym=false brace=false paren=true bracket=false var=false: subst("${bb}")="${bb}"
    sym=false brace=false paren=true bracket=false var=true: subst("${bb}")="${bb}"
    sym=false brace=false paren=true bracket=true var=false: subst("${bb}")="${bb}"
    sym=false brace=false paren=true bracket=true var=true: subst("${bb}")="${bb}"
    sym=false brace=true paren=false bracket=false var=false: subst("${bb}")="BB!"
    sym=false brace=true paren=false bracket=false var=true: subst("${bb}")="BB!"
    sym=false brace=true paren=false bracket=true var=false: subst("${bb}")="BB!"
    sym=false brace=true paren=false bracket=true var=true: subst("${bb}")="BB!"
    sym=false brace=true paren=true bracket=false var=false: subst("${bb}")="BB!"
    sym=false brace=true paren=true bracket=false var=true: subst("${bb}")="BB!"
    sym=false brace=true paren=true bracket=true var=false: subst("${bb}")="BB!"
    sym=false brace=true paren=true bracket=true var=true: subst("${bb}")="BB!"
    sym=true brace=false paren=false bracket=false var=false: subst("${bb}")="${bb}"
    sym=true brace=false paren=false bracket=false var=true: subst("${bb}")="${bb}"
    sym=true brace=false paren=false bracket=true var=false: subst("${bb}")="${bb}"
    sym=true brace=false paren=false bracket=true var=true: subst("${bb}")="${bb}"
    sym=true brace=false paren=true bracket=false var=false: subst("${bb}")="${bb}"
    sym=true brace=false paren=true bracket=false var=true: subst("${bb}")="${bb}"
    sym=true brace=false paren=true bracket=true var=false: subst("${bb}")="${bb}"
    sym=true brace=false paren=true bracket=true var=true: subst("${bb}")="${bb}"
    sym=true brace=true paren=false bracket=false var=false: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=false bracket=false var=true: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=false bracket=true var=false: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=false bracket=true var=true: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=true bracket=false var=false: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=true bracket=false var=true: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=true bracket=true var=false: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=true brace=true paren=true bracket=true var=true: subst("${bb}") raised Ez_subst.UnclosedExpression("bb")
    sym=false brace=false paren=false bracket=false var=false: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${bb}{}$")="${bb}{}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=false bracket=false var=true: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=false bracket=true var=false: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=false bracket=true var=true: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=true bracket=false var=false: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=true bracket=false var=true: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=true bracket=true var=false: subst("${bb}{}$")="BB!{}$"
    sym=false brace=true paren=true bracket=true var=true: subst("${bb}{}$")="BB!{}$"
    sym=true brace=false paren=false bracket=false var=false: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${bb}{}$")="${bb}{}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=false bracket=false var=true: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=false bracket=true var=false: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=false bracket=true var=true: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=true bracket=false var=false: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=true bracket=false var=true: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=true bracket=true var=false: subst("${bb}{}$")="<{bb}{}>"
    sym=true brace=true paren=true bracket=true var=true: subst("${bb}{}$")="<{bb}{}>"
    sym=false brace=false paren=false bracket=false var=false: subst("${")="${"
    sym=false brace=false paren=false bracket=false var=true: subst("${")="${"
    sym=false brace=false paren=false bracket=true var=false: subst("${")="${"
    sym=false brace=false paren=false bracket=true var=true: subst("${")="${"
    sym=false brace=false paren=true bracket=false var=false: subst("${")="${"
    sym=false brace=false paren=true bracket=false var=true: subst("${")="${"
    sym=false brace=false paren=true bracket=true var=false: subst("${")="${"
    sym=false brace=false paren=true bracket=true var=true: subst("${")="${"
    sym=false brace=true paren=false bracket=false var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=false bracket=false var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=false bracket=true var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=false bracket=true var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=true bracket=false var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=true bracket=false var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=true bracket=true var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=true paren=true bracket=true var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=false paren=false bracket=false var=false: subst("${")="${"
    sym=true brace=false paren=false bracket=false var=true: subst("${")="${"
    sym=true brace=false paren=false bracket=true var=false: subst("${")="${"
    sym=true brace=false paren=false bracket=true var=true: subst("${")="${"
    sym=true brace=false paren=true bracket=false var=false: subst("${")="${"
    sym=true brace=false paren=true bracket=false var=true: subst("${")="${"
    sym=true brace=false paren=true bracket=true var=false: subst("${")="${"
    sym=true brace=false paren=true bracket=true var=true: subst("${")="${"
    sym=true brace=true paren=false bracket=false var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=false bracket=false var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=false bracket=true var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=false bracket=true var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=true bracket=false var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=true bracket=false var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=true bracket=true var=false: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=true brace=true paren=true bracket=true var=true: subst("${") raised Ez_subst.UnclosedExpression("")
    sym=false brace=false paren=false bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=false var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=false var=true: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=true var=false: subst("\\${a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=true var=true: subst("\\${a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=false brace=true paren=true bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=false var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=false var=true: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=true var=false: subst("$\\{a}}$")="${a}}$"
    sym=true brace=true paren=true bracket=true var=true: subst("$\\{a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=false: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${\\a}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${\\a}}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=false bracket=false var=true: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=false bracket=true var=false: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=false bracket=true var=true: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=true bracket=false var=false: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=true bracket=false var=true: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=true bracket=true var=false: subst("${\\a}}$")="AA!}$"
    sym=false brace=true paren=true bracket=true var=true: subst("${\\a}}$")="AA!}$"
    sym=true brace=false paren=false bracket=false var=false: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${\\a}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${\\a}}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=false bracket=false var=true: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=false bracket=true var=false: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=false bracket=true var=true: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=false var=false: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=false var=true: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=true var=false: subst("${\\a}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=true var=true: subst("${\\a}}$")="<{a}}>"
    sym=false brace=false paren=false bracket=false var=false: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${a\\}}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${a\\}}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=false bracket=false var=true: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=false bracket=true var=false: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=false bracket=true var=true: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=true bracket=false var=false: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=true bracket=false var=true: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=true bracket=true var=false: subst("${a\\}}$")="<{a}}>$"
    sym=false brace=true paren=true bracket=true var=true: subst("${a\\}}$")="<{a}}>$"
    sym=true brace=false paren=false bracket=false var=false: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${a\\}}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${a\\}}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=false bracket=false var=true: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=false bracket=true var=false: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=false bracket=true var=true: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=false var=false: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=false var=true: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=true var=false: subst("${a\\}}$")="<{a}}>"
    sym=true brace=true paren=true bracket=true var=true: subst("${a\\}}$")="<{a}}>"
    sym=false brace=false paren=false bracket=false var=false: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${a}\\}$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${a}\\}$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=false bracket=false var=true: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=false bracket=true var=false: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=false bracket=true var=true: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=true bracket=false var=false: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=true bracket=false var=true: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=true bracket=true var=false: subst("${a}\\}$")="AA!}$"
    sym=false brace=true paren=true bracket=true var=true: subst("${a}\\}$")="AA!}$"
    sym=true brace=false paren=false bracket=false var=false: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${a}\\}$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${a}\\}$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=false bracket=false var=true: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=false bracket=true var=false: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=false bracket=true var=true: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=false var=false: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=false var=true: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=true var=false: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=true var=true: subst("${a}\\}$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=false brace=false paren=false bracket=false var=false: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=false bracket=false var=true: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=false: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=false bracket=true var=true: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=false: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=true bracket=false var=true: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=false: subst("${a}}\\$")="${a}}$"
    sym=false brace=false paren=true bracket=true var=true: subst("${a}}\\$")="${a}}$"
    sym=false brace=true paren=false bracket=false var=false: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=false bracket=false var=true: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=false bracket=true var=false: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=false bracket=true var=true: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=true bracket=false var=false: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=true bracket=false var=true: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=true bracket=true var=false: subst("${a}}\\$")="AA!}$"
    sym=false brace=true paren=true bracket=true var=true: subst("${a}}\\$")="AA!}$"
    sym=true brace=false paren=false bracket=false var=false: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=false bracket=false var=true: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=false: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=false bracket=true var=true: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=false: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=true bracket=false var=true: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=false: subst("${a}}\\$")="${a}}$"
    sym=true brace=false paren=true bracket=true var=true: subst("${a}}\\$")="${a}}$"
    sym=true brace=true paren=false bracket=false var=false: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=false bracket=false var=true: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=false bracket=true var=false: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=false bracket=true var=true: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=false var=false: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=false var=true: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=true var=false: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$")
    sym=true brace=true paren=true bracket=true var=true: subst("${a}}\\$") raised Ez_subst.UnclosedExpression("a}}$") |}]
