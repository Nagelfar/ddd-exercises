module Exercise2

type Destination =
    | A
    | B

type CargoIdentifier = Identifier of int

type Cargo = CargoIdentifier * Destination

type Location =
    | Factory
    | Port
    | Warehouse of Destination

type TransportIdentifier = Identifier of int
type Vehicle =
    | Truck of TransportIdentifier
    | Ship of TransportIdentifier

type Timepoint = int



type Event =
    | CargoReadyForDelivery of Cargo
    | VehicleProvided of Vehicle * Location
    | Departing of Vehicle * from: Location * Cargo list * destination: Location
    | PlannedArrival of Vehicle * destination: Location * Cargo list
    | ParkedShipment of Cargo * Location
    | PickedUpShipment of Cargo * Location
    | DeliveredShipment of Cargo

type Entry = Event * Timepoint

module Domain =
    let pickUpCargoAtFactory time cargos truck =
        match cargos |> List.tryHead with
        | Some ((_, A) as c) ->
            [ Entry(PickedUpShipment(c, Factory), time)
              Entry(Departing(Truck truck, Factory, [ c ], Port), time)
              Entry(PlannedArrival(Truck truck, Port, [ c ]), time + 1)
              Entry(ParkedShipment(c, Port), time + 1)
              Entry(Departing(Truck truck, Port, [], Factory), time + 1)
              Entry(PlannedArrival(Truck truck, Factory, []), time + 2) ]
        | Some ((_, B) as c) ->
            [ Entry(PickedUpShipment(c, Factory), time)
              Entry(Departing(Truck truck, Factory, [ c ], Warehouse B), time)
              Entry(PlannedArrival(Truck truck, Warehouse B, [ c ]), time + 5)
              Entry(DeliveredShipment(c), time + 5)
              Entry(Departing(Truck truck, Warehouse B, [], Factory), time + 5)
              Entry(PlannedArrival(Truck truck, Factory, []), time + 10) ]
        | None -> []          

    let pickUpCargoAtPort time cargos ship =
        if Seq.isEmpty cargos then
            []
        else
            let travelSpeed = 6
            let unLoadingSpeed = 1
            let capacity = 4
            let transportableCargo = cargos |> List.truncate capacity
            let pickingUpShipments =
                transportableCargo 
                |> List.map(fun c -> Entry(PickedUpShipment(c,Port), time))
            let deliveredShipments = 
                transportableCargo
                |> List.map (fun c -> Entry(DeliveredShipment(c), time + unLoadingSpeed + travelSpeed + unLoadingSpeed))

            pickingUpShipments 
                @ [ Entry(Departing(ship, Port, transportableCargo, Warehouse A), time + unLoadingSpeed)
                    Entry(PlannedArrival(ship, Warehouse A, transportableCargo), time +  unLoadingSpeed + travelSpeed)]
                @ deliveredShipments
                @ [ Entry(Departing(ship, Warehouse A, [], Port), time + unLoadingSpeed + travelSpeed + unLoadingSpeed)
                    Entry(PlannedArrival(ship, Port, []), time + unLoadingSpeed + travelSpeed + unLoadingSpeed + travelSpeed)]


module Projections =
    let findLatestDelivery events =
        events
        |> Seq.choose (function
            | DeliveredShipment _, t -> Some t
            | _ -> None)
        |> Seq.max

    let filterUntilNow time events =
        events
        |> Seq.filter (fun (_, t) -> t <= time)
        |> Seq.map fst

    let aggregate folder (time: Timepoint) (events: Entry list) =
        events
        |> filterUntilNow time
        |> Seq.fold folder []

    let trucksAtFactory =
        aggregate (fun s e ->
            match e with
            | Departing(Truck t, Factory, _, _) -> List.except [ t ] s
            | PlannedArrival(Truck t, Factory, _)
            | VehicleProvided(Truck t, Factory) -> s @ [ t ]
            | _ -> s)

    let cargoAtFactory =
        aggregate (fun s e ->
            match e with
            | CargoReadyForDelivery c -> s @ [ c ]
            | PickedUpShipment(c, Factory) -> List.except [ c ] s
            | _ -> s)

    let cargoWaitingOnPort =
        aggregate (fun s e ->
            match e with
            | ParkedShipment(c, Port) -> s @ [ c ]
            | PickedUpShipment(c, Port) -> List.except [ c ] s
            | _ -> s)

    let avaliableShip =
        aggregate (fun s e -> 
            match e with
            | VehicleProvided(Ship ship, Port)
            | PlannedArrival(Ship ship, Port, _) -> s @ [ Ship ship ]
            | Departing(Ship ship, Port, _, _) -> List.except [Ship ship] s
            | _ -> s)

open Projections

let moveCargoFromPort time events =
    let cargo = cargoWaitingOnPort time events
    let vehicle = avaliableShip time events |> List.tryHead
    vehicle
    |> Option.map (Domain.pickUpCargoAtPort time cargo)
    |> Option.defaultValue []

let moveCargoFromFactory time events =
    let cargo = cargoAtFactory time events
    let vehicle = trucksAtFactory time events |> List.tryHead
    vehicle
    |> Option.map (Domain.pickUpCargoAtFactory time cargo)
    |> Option.defaultValue []

let step time events =
    // TODO: there might be a racing condition?
    // What should happen if at the same timepoint a truck unloads some cargo - can a ship pick it up?
    let eventsFromPort = moveCargoFromPort time events
    let mutable mEvents = events @ eventsFromPort
    let mutable tryMoveCargo = true
    while tryMoveCargo do
        let eventsFromFactory = moveCargoFromFactory time mEvents
        tryMoveCargo <- not eventsFromFactory.IsEmpty
        mEvents <- mEvents @ eventsFromFactory

    mEvents

let iterate intialEvents =
    let mutable events: Entry list = intialEvents
    let mutable finished = false
    let mutable time = 0
    while not finished do
        events <- step time events
        time <- time + 1
        finished <- (cargoAtFactory time events).IsEmpty && (cargoWaitingOnPort time events).IsEmpty
    events

let buildInitialEvents cargo =
    let initialCargo = cargo |> List.mapi (fun i c -> CargoReadyForDelivery(CargoIdentifier.Identifier i, c))

    let initialVehicles =
        [ VehicleProvided(Truck (Identifier 1), Factory)
          VehicleProvided(Truck (Identifier 2), Factory)
          VehicleProvided(Ship (Identifier 3), Port) ]

    List.concat [ initialCargo; initialVehicles ] |> List.map (fun e -> e, 0)

module Program =
    open System.Text.Json
    open System.Text.Json.Serialization
    open Microsoft.FSharp.Core.Printf

    type CargoEntry =
        { Cargo_id: int
          Destination: string
          Origin: string }

    type EventEntry =
        { Event: string
          Time: int
          Transport_id: int
          Kind: string
          Location: string
          Destination: string
          Cargo: CargoEntry list }

    let transportId =
        function
        | Truck (Identifier t) -> t
        | Ship (Identifier s) -> s 

    let convertVehicle =
        function
        | Truck _ -> "TRUCK"
        | Ship _ -> "SHIP"

    let convertLocation =
        function
        | Factory -> "FACTORY"
        | Port -> "PORT"
        | Warehouse w -> string w

    let convertCargo (CargoIdentifier.Identifier(id), d) =
        { Cargo_id = id
          Destination = string d
          Origin = convertLocation Factory }

    let convertToTrace events =
        events
        |> Seq.sortBy snd
        |> Seq.choose (function
            | Departing(v, f, c, d), time ->
                { Event = "DEPART"
                  Time = time
                  Transport_id = transportId v
                  Kind = convertVehicle v
                  Location = convertLocation f
                  Destination = convertLocation d
                  Cargo = c |> List.map convertCargo }
                |> Some
            | PlannedArrival(v, d, c), time ->
                { Event = "ARRIVE"
                  Time = time
                  Transport_id = transportId v
                  Kind = convertVehicle v
                  Location = convertLocation d
                  Destination = null
                  Cargo = c |> List.map convertCargo }
                |> Some
            | _ -> None)


    let parseInput (input: string) =
        input.ToCharArray()
        |> Seq.map (function
            | 'A' -> A
            | 'B' -> B
            | x -> failwithf "Unknown cargo %c" x)

    let serialize v =
        let options = JsonSerializerOptions(PropertyNamingPolicy = JsonNamingPolicy.CamelCase, IgnoreNullValues = true)
        JsonSerializer.Serialize(v, options)

    let traceOutput events cargo =
        use writer = new System.IO.StringWriter()
        let appendLine format = fprintfn writer format
        let highest = events |> findLatestDelivery
        appendLine "# Deliver cargo %A within %i" cargo highest
        events
        |> convertToTrace
        |> Seq.map serialize
        |> Seq.iter (appendLine "%s")
        writer.ToString()

    [<EntryPoint>]
    let main argv =
        if argv.Length < 1 then failwithf "Expecting at least a single string containing the cargo list but got %A" argv

        let cargo = parseInput argv.[0] |> Seq.toList

        let events =
            cargo
            |> buildInitialEvents
            |> iterate

        if argv.Length = 1 then printfn "No explicit arguments given, fallback to time"
        if Array.contains "time" argv || argv.Length = 1 then
            let highest = events |> findLatestDelivery
            printfn "Highest time %A for route %A" highest cargo
        if Array.contains "events" argv then printfn "%A" events
        if Array.contains "trace" argv then traceOutput events cargo |> printf "%s"

        0 // return an integer exit code
