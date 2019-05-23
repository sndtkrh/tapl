open Lambdacalc

let () =
    let ctx = ["x"] in
    let t =
      Lambdacalc.TmApp("", 
        Lambdacalc.TmAbs("", "x",
          Lambdacalc.TmApp("",
            Lambdacalc.TmVar("", 0, 2),
            Lambdacalc.TmVar("", 0, 2))),
        Lambdacalc.TmAbs("", "x",
          Lambdacalc.TmVar("", 0, 2))) in
    Printf.printf "%s\n" (Lambdacalc.printtm ctx (Lambdacalc.eval ctx t))

(*
let () =
        let ctx = ["x"] in
    let t =
        Lambdacalc.TmVar("", 0, 1) in 
    Printf.printf "%s\n" (Lambdacalc.printtm ctx t)
*)
