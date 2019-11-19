#load "Exercise2.fs"

open Exercise2

let test cargo expected =
    let highest =
        cargo
        |> buildInitialState
        |> iterate
        |> findLatestDelivery
    if highest = expected then
        printfn "For cargo %A: got expected time %A" cargo highest
    else
        eprintfn "FAILING: For cargo %A: expected time %A but got %A" cargo expected highest


test [A;] 5
test [A;B;] 5
test [B;B;] 5
test [A;B;B;] 7
test [A;A;B;A;B;B;A;B;] 29
test [A;B;B;B;A;B;A;A;A;B;B;B;] 41
