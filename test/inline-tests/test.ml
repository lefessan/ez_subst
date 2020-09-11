
let sub = Ez_subst.string
    ~sep:'%'
            ~brace:`Brace
            ~sym:true
    (fun context s ->
  match s with
  | "toto" -> "TOTO"
  | "tutu" -> context
  | x -> "<" ^ x ^ ">") "to"

let sub_error s err_s =
  try
    ignore (sub s);
    false
  with Ez_subst.UnclosedExpression err ->
    err = err_s

let%test "1" = sub "%{toto}%" = "TOTO"
let%test "2" = sub "%{tutu}%" = "to"
let%test "3" = sub "%{toto}%%{tutu}%%{toto}%" = "TOTOtoTOTO"
let%test "4" = sub "%{to%{tutu}%}%%{tutu}%%{toto}%" = "TOTOtoTOTO"
let%test "5" = sub_error "%{toto" "toto"
let%test "6" = sub_error "%{toto}" "toto"
let%test "7" = sub "%{toto}{}%" = "<toto}{>"
let%test "8" = sub_error "%{" ""
let%test "9" = sub "%%" = "%"
