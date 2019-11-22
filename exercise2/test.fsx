#I @"/Users/fm/.nuget/packages/"
#r "system.text.json/4.6.0/lib/net461/System.Text.Json.dll"
#r "system.text.encodings.web/4.0.0/lib/netstandard1.0/System.Text.Encodings.Web.dll"
#load "Exercise2.fs"

open Exercise2

let test cargo expected =
    let events =
        cargo
        |> buildInitialEvents
        |> iterate
    let highest =
        events
        |> Projections.findLatestDelivery

    if highest = expected then
        printfn "For cargo %A: got expected time %A" cargo highest
        let filename = System.String.Join("", cargo) + ".log"
        let output = Program.traceOutput events cargo
        System.IO.File.WriteAllText(System.IO.Path.Combine("output",filename), output)
    else
        eprintfn "FAILING: For cargo %A: expected time %A but got %A" cargo expected highest


test [A;] 5
test [A;B;] 5
test [B;B;] 5
test [A;B;B;] 7
test [A;A;B;A;B;B;A;B;] 29
test [A;B;B;B;A;B;A;A;A;B;B;B;] 41
