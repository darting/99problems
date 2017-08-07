open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

let addE = <@@ fun a b -> a + b @@>

let rec evalExpr q =
    match q with
    | SpecificCall <@ (+) @> (_, _, [a; b]) -> 
        let l, r = evalExpr a, evalExpr b
        l + r
    | SpecificCall <@ (*) @> (_, _, [a; b]) -> 
        let l, r = evalExpr a, evalExpr b
        l * r
    | Value (v, t) ->
        if t = typeof<int> then v :?> int 
            else failwith "Only integer values supported"
    | _  -> failwith "Unsupported expression"

evalExpr <@ 22 + 2 * 22 + 45 @>

