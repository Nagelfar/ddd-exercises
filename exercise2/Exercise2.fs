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

type Truck =
    | T1
    | T2

type Vehicle =
    | Truck of Truck
    | Ship

type Timepoint = int

type TransportIdentifier = Identifier of int

type Event =
    | CargoReadyForDelivery of Cargo
    | VehicleProvided of Vehicle * Location
    | TransportCreated of TransportIdentifier
    | Departing of TransportIdentifier * Vehicle * from: Location * Cargo option * destination: Location
    | PlannedArrival of TransportIdentifier * Vehicle * destination: Location * Cargo option
    | ParkedShipment of Cargo * Location
    | PickedUpShipment of Cargo * Location
    | DeliveredShipment of Cargo

type Entry = Event * Timepoint

module Domain =
    let pickUpCargoAtFactory time transportId cargo truck =
        match cargo with
        | (_, A) ->
            [ Entry(TransportCreated(transportId), time)
              Entry(PickedUpShipment(cargo, Factory), time)
              Entry(Departing(transportId, Truck truck, Factory, Some cargo, Port), time)
              Entry(PlannedArrival(transportId, Truck truck, Port, Some cargo), time + 1)
              Entry(ParkedShipment(cargo, Port), time + 1)
              Entry(Departing(transportId, Truck truck, Port, None, Factory), time + 1)
              Entry(PlannedArrival(transportId, Truck truck, Factory, None), time + 2) ]
        | (_, B) ->
            [ Entry(TransportCreated(transportId), time)
              Entry(PickedUpShipment(cargo, Factory), time)
              Entry(Departing(transportId, Truck truck, Factory, Some cargo, Warehouse B), time)
              Entry(PlannedArrival(transportId, Truck truck, Warehouse B, Some cargo), time + 5)
              Entry(DeliveredShipment(cargo), time + 5)
              Entry(Departing(transportId, Truck truck, Warehouse B, None, Factory), time + 5)
              Entry(PlannedArrival(transportId, Truck truck, Factory,None), time + 10) ]

    let pickUpCargoAtPort time transportId cargo ship =
        [ Entry(TransportCreated(transportId), time)
          Entry(PickedUpShipment(cargo, Port), time)
          Entry(Departing(transportId, ship, Port, Some cargo, Warehouse A), time)
          Entry(PlannedArrival(transportId, ship, Warehouse A,Some cargo), time + 4) 
          Entry(DeliveredShipment(cargo), time + 4)
          Entry(Departing(transportId, ship, Warehouse A, None, Port), time + 4)
          Entry(PlannedArrival(transportId, ship, Port,None), time + 8) ]

module Projections =
    let findLatestDelivery events =
        events
        |> Seq.choose (function
            | DeliveredShipment _, t -> Some t
            | _ -> None)
        |> Seq.max

    let nextTransportId events =
        events
        |> Seq.map fst
        |> Seq.choose (function
            | TransportCreated(Identifier id) -> Some id
            | _ -> None)
        |> Seq.tryLast
        |> Option.map (fun v -> v + 1)
        |> Option.defaultValue 0
        |> Identifier

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
            | Departing(_, Truck t, Factory, _, _) -> List.except [ t ] s
            | PlannedArrival(_, Truck t, Factory,_)
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

    let avaliableShip time events =
        events
        |> filterUntilNow time
        |> Seq.choose (function
            | VehicleProvided(Ship, Port)
            | PlannedArrival(_, Ship, Port,_) -> Some <| Some Ship
            | Departing(_, Ship, Port, _, _) -> Some None
            | _ -> None)
        |> Seq.tryLast
        |> Option.flatten

open Projections

let moveCargoFromPort time events =
    let cargo = cargoWaitingOnPort time events |> List.tryHead
    let vehicle = avaliableShip time events
    let nextId = nextTransportId events
    Option.map2 (Domain.pickUpCargoAtPort time nextId) cargo vehicle |> Option.defaultValue []

let moveCargoFromFactory time events =
    let cargo = cargoAtFactory time events |> List.tryHead
    let vehicle = trucksAtFactory time events |> List.tryHead
    let nextId = nextTransportId events
    Option.map2 (Domain.pickUpCargoAtFactory time nextId) cargo vehicle |> Option.defaultValue []

let step time events =
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
        [ VehicleProvided(Truck T1, Factory)
          VehicleProvided(Truck T2, Factory)
          VehicleProvided(Ship, Port) ]

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

    let convertVehicle =
        function
        | Truck _ -> "TRUCK"
        | Ship -> "SHIP"

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
            | Departing(Identifier(id), v, f, c, d), time ->
                { Event = "DEPART"
                  Time = time
                  Transport_id = id
                  Kind = convertVehicle v
                  Location = convertLocation f
                  Destination = convertLocation d
                  Cargo = c |> Option.map convertCargo |> Option.toList }
                |> Some
            | PlannedArrival(Identifier(id), v, d, c), time ->
                { Event = "ARRIVE"
                  Time = time
                  Transport_id = id
                  Kind = convertVehicle v
                  Location = convertLocation d
                  Destination = null
                  Cargo = c |> Option.map convertCargo |> Option.toList  }
                |> Some
            | _ -> None)
                

    let parseInput (input: string ) =
        input.ToCharArray()
        |> Seq.map (function
            | 'A' -> A
            | 'B' -> B
            | x -> failwithf "Unknown cargo %c" x)

    let serialize v =
        let options = JsonSerializerOptions(PropertyNamingPolicy=JsonNamingPolicy.CamelCase)
        JsonSerializer.Serialize (v , options)

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
        if argv.Length < 1 then
            failwithf "Expecting at least a single string containing the cargo list but got %A" argv

        let cargo = parseInput argv.[0] |> Seq.toList

        let events =
            cargo
            |> buildInitialEvents
            |> iterate

        if argv.Length = 1 then
            printfn "No explicit arguments given, fallback to time"
        if Array.contains "time" argv || argv.Length = 1 then
            let highest = events |> findLatestDelivery
            printfn "Highest time %A for route %A" highest cargo
        if Array.contains "events" argv then
            printfn "%A" events
        if Array.contains "trace" argv then
            traceOutput events cargo |> printf "%s"

        0 // return an integer exit code
