#load "../.paket/load/netstandard2.0/main.group.fsx"
#load "Exercise2.fs"

open Exercise2

let test cargo expected =
    let events =
        cargo
        |> buildInitialEvents
        |> iterate

    let highest = events |> Projections.findLatestDelivery

    if highest = expected then
        printfn "For cargo %A: got expected time %A" cargo highest
        let filename = System.String.Join("", cargo) + ".log"
        let output = Program.traceOutput events cargo
        System.IO.File.WriteAllText(System.IO.Path.Combine("output", filename), output)
    else
        eprintfn "FAILING: For cargo %A: expected time %A but got %A" cargo expected highest


(**
    FACTORY---TRUCK---PORT---SHIP---A
    |           1              4
    T
    R
    U 5
    C
    K
    |
    B
*)
let example1() =
    test [ A ] 5
    test [ A; B ] 5
    test [ B; B ] 5
    test [ A; B; B ] 7
    test [ A; A; B; A; B; B; A; B ] 29
    test [ A; B; B; B; A; B; A; A; A; B; B; B ] 41


(**
    FACTORY---TRUCK---PORT-L--SHIP---U-A
    |           1          1    6    1
    T                   Loading  Unloading
    R
    U 5
    C
    K
    |
    B
*)
let example2() =
    test [ A ] 9
    test [ A; B ] 9
    test [ A; A ] 9
    test [ B; B ] 5
    test [ A; B; B ] 9
    test [ A; A; B; A; B; B; A; B ] 41
    test [ A; B; B; B; A; B; A; A; A; B; B; B ] 41


example2()
