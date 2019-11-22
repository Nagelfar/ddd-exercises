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

type Truck = TransportIdentifier

type Ship = TransportIdentifier

type Vehicle =
    | Truck of Truck
    | Ship of Ship

type Timepoint = int

type Event =
    | CargoReadyForDelivery of Cargo * Location
    | VehicleProvided of Vehicle * Location
    | Departing of Vehicle * from: Location * Cargo list * destination: Location
    | PlannedArrival of Vehicle * destination: Location * Cargo list
    | MovedShipment of Cargo * Location
    | PickedUpShipment of Cargo * Location

type Entry = Event * Timepoint

module Domain =

    module Model =

        let travelTime location1 location2 =
            match location1, location2 with
            | Factory, Port -> 1
            | Factory, Warehouse B -> 5
            | Port, Warehouse A -> 6
            | _ -> failwithf "Traveltime not configured for %A to %A" location1 location2

        let unLoadingTime =
            function
            | Truck _ -> 0
            | Ship _ -> 1

        let vehicleCapacity =
            function
            | Truck _ -> 1
            | Ship _ -> 4

        let initialVehicles =
            [ VehicleProvided(Truck(Identifier 1), Factory)
              VehicleProvided(Truck(Identifier 2), Factory)
              VehicleProvided(Ship(Identifier 3), Port) ]

    let findVehicleType filter vehichles =
        vehichles
        |> List.choose filter
        |> List.tryHead

    let findShip =
        function
        | Ship s -> Some s
        | _ -> None

    let findTruck =
        function
        | Truck t -> Some t
        | _ -> None

    let transportLeg time cargos vehicle origin destination =
        let travelSpeed = Model.travelTime origin destination
        let unLoadingSpeed = Model.unLoadingTime vehicle
        let capacity = Model.vehicleCapacity vehicle
        let transportableCargo = cargos |> List.truncate capacity

        let pickingUpShipments = transportableCargo |> List.map (fun c -> Entry(PickedUpShipment(c, origin), time))

        let travelToDestination =
            [ Entry(Departing(vehicle, origin, transportableCargo, destination), time + unLoadingSpeed)
              Entry(PlannedArrival(vehicle, destination, transportableCargo), time + unLoadingSpeed + travelSpeed) ]

        let deliveringShipments =
            transportableCargo
            |> List.map
                (fun c -> Entry(MovedShipment(c, destination), time + unLoadingSpeed + travelSpeed + unLoadingSpeed))

        let travelBackToOrigin =
            [ Entry(Departing(vehicle, destination, [], origin), time + unLoadingSpeed + travelSpeed + unLoadingSpeed)
              Entry
                  (PlannedArrival(vehicle, origin, []),
                   time + unLoadingSpeed + travelSpeed + unLoadingSpeed + travelSpeed) ]

        pickingUpShipments @ travelToDestination @ deliveringShipments @ travelBackToOrigin

    let pickUpCargoAtFactory time cargos vehicles =
        let potentialTruck = findVehicleType findTruck vehicles
        match cargos |> List.tryHead, potentialTruck with
        | Some((_, A) as c), Some truck -> transportLeg time [ c ] (Truck truck) Factory Port
        | Some((_, B) as c), Some truck -> transportLeg time [ c ] (Truck truck) Factory (Warehouse B)
        | _ -> []

    let pickUpCargoAtPort time cargos vehichles =
        let potentialShip = findVehicleType findShip vehichles
        match cargos, potentialShip with
        | cargo, Some ship when not <| List.isEmpty cargo -> transportLeg time cargo (Ship ship) Port (Warehouse A)
        | _ -> []

module Projections =
    let findLatestDelivery events =
        events
        |> Seq.choose (function
            | MovedShipment(_, Warehouse _), t -> Some t
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

    let cargoAt location =
        aggregate (fun s e ->
            match e with
            | CargoReadyForDelivery(c, l)
            | MovedShipment(c, l) when l = location -> s @ [ c ]
            | PickedUpShipment(c, l) when l = location -> List.except [ c ] s
            | _ -> s)

    let vehicleAt location =
        aggregate (fun s e ->
            match e with
            | Departing(t, l, _, _) when l = location -> List.except [ t ] s
            | PlannedArrival(t, l, _)
            | VehicleProvided(t, l) when l = location -> s @ [ t ]
            | _ -> s)

    let cargoToBeMoved events =
        events
        |> List.map fst
        |> List.sumBy (function
            | CargoReadyForDelivery _ -> 1
            | MovedShipment(_, Warehouse _) -> -1
            | _ -> 0)

let moveCargoFrom location mover time events =
    let cargo = Projections.cargoAt location time events
    let vehicle = Projections.vehicleAt location time events
    mover time cargo vehicle

let rec moveCargoFromFactory time events =
    let movedEvents = moveCargoFrom Factory Domain.pickUpCargoAtFactory time events

    if movedEvents.IsEmpty then events
    else moveCargoFromFactory time (events @ movedEvents)

let step time events =
    // TODO: there might be a racing condition?
    // What should happen if at the same time point a truck unloads some cargo - can a ship pick it up?
    // Currently it depends wether we pass `events` or `events @ eventsFromPort` to moveCargoFromFactory
    let eventsFromPort = moveCargoFrom Port Domain.pickUpCargoAtPort time events
    moveCargoFromFactory time (events @ eventsFromPort)

let rec iterate time events =
    let newEvents = step time events
    match Projections.cargoToBeMoved newEvents with
    | 0 -> newEvents
    | _ -> iterate (time + 1) newEvents

let buildInitialEvents cargoDestination =
    let initialCargo =
        cargoDestination |> List.mapi (fun i c -> CargoReadyForDelivery((CargoIdentifier.Identifier i, c), Factory))

    List.concat [ initialCargo; Domain.Model.initialVehicles ] |> List.map (fun e -> e, 0)

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
        | Truck(Identifier t) -> t
        | Ship(Identifier s) -> s

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
        let highest = events |> Projections.findLatestDelivery
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
            |> iterate 0

        if argv.Length = 1 then printfn "No explicit arguments given, fallback to time"
        if Array.contains "time" argv || argv.Length = 1 then
            let highest = events |> Projections.findLatestDelivery
            printfn "Highest time %A for route %A" highest cargo
        if Array.contains "events" argv then printfn "%A" events
        if Array.contains "trace" argv then traceOutput events cargo |> printf "%s"

        0 // return an integer exit code
