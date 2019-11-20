module Exercise2

type Destination = A | B

type CargoIdentifier = Identifier of int
type Cargo = CargoIdentifier * Destination

type Location =
    | Factory
    | Port
    | Warehouse of Destination

type Truck = T1 | T2
type Vehicle =
    | Truck of Truck
    | Ship

type Timepoint = int

type TransportIdentifier = Identifier of int

type Event =
    | CargoReadyForDelivery of Cargo
    | VehicleProvided of Vehicle * Location
    | TransportCreated of TransportIdentifier
    | Departing of TransportIdentifier * Vehicle * from: Location * Cargo * destination: Location
    | Arrived of TransportIdentifier * Vehicle * Location * Cargo
    | ArrivedBack of TransportIdentifier * Vehicle * destination: Location
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
              Entry(Departing(transportId, Truck truck, Factory, cargo, Port), time)
              Entry(ParkedShipment(cargo, Port), time + 1)
              Entry(ArrivedBack(transportId, Truck truck, Factory), time + 2) ]
        | (_, B) ->
            [ Entry(TransportCreated(transportId), time)
              Entry(PickedUpShipment(cargo, Factory), time)
              Entry(Departing(transportId, Truck truck, Factory, cargo, Warehouse B), time)
              Entry(DeliveredShipment(cargo), time + 5)
              Entry(ArrivedBack(transportId, Truck truck, Factory), time + 10) ]

    let pickUpCargoAtPort time transportId cargo ship =
        [ Entry(TransportCreated(transportId), time)
          Entry(PickedUpShipment(cargo, Port), time)
          Entry(Departing(transportId, ship, Port, cargo, Warehouse A), time)
          Entry(DeliveredShipment(cargo), time + 4)
          Entry(ArrivedBack(transportId, ship, Port), time + 8) ]

type State =
    { 
      NextTransportId: TransportIdentifier }
    static member Initial =
          { 
            NextTransportId = Identifier(0) }

let update (state: State) event =
    match event with
    | TransportCreated(Identifier(id)), _ -> { state with NextTransportId = Identifier(id + 1) }
    
    | _ -> state

let buildState events = events |> Seq.fold update State.Initial

let filterUntilNow time events =
    events
    |> Seq.filter (fun (_, t) -> t <= time)
    |> Seq.map fst

let trucksAtFactory time events =
    events
    |> filterUntilNow time
    |> Seq.fold (fun s e ->
        match e with
        | Departing(_, Truck t, Factory, _, _) -> List.except [t] s
        | ArrivedBack(_, Truck t, Factory)
        | VehicleProvided(Truck t, Factory)-> s @ [t]
        | _ -> s
    ) []

let cargoAtFactory time events =
    events
    |> filterUntilNow time
    |> Seq.fold (fun s e ->
        match e with
        | CargoReadyForDelivery c -> s @ [c]
        | PickedUpShipment(c, Factory) -> List.except [c] s
        | _ -> s
    ) []

let avaliableShip time events =
    events
    |> filterUntilNow time
    |> Seq.choose (function
        | VehicleProvided(Ship, Port)
        | ArrivedBack(_, Ship, Port) -> Some <| Some Ship
        | Departing(_, Ship, Port, _, _) -> Some None
        | _ -> None)
    |> Seq.tryLast
    |> Option.flatten

let cargoWaitingOnPort time events = 
    events
    |> filterUntilNow time
    |> Seq.fold (fun s e -> 
        match e with
        | ParkedShipment(c,Port) -> s @ [c]
        | PickedUpShipment(c,Port) -> List.except [c] s
        | _ -> s
    ) []

let moveCargoFromPort time events =
    let state = buildState events
    let cargo = 
        events
        |> cargoWaitingOnPort time 
        |> List.tryHead

    let vehicle = avaliableShip time events
    Option.map2 (Domain.pickUpCargoAtPort time state.NextTransportId) cargo vehicle |> Option.defaultValue []

let moveCargoFromFactory time events =
    let state = buildState events
    let cargo = cargoAtFactory time events |> List.tryHead 
    let vehicle = trucksAtFactory time events |> List.tryHead
    Option.map2 (Domain.pickUpCargoAtFactory time state.NextTransportId) cargo vehicle |> Option.defaultValue []
    
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
        let state = buildState events
        finished <- (cargoAtFactory time events).IsEmpty && (cargoWaitingOnPort time events).IsEmpty
    events

let findLatestDelivery events =
    events
    |> Seq.choose (function
        | DeliveredShipment _, t -> Some t
        | _ -> None)
    |> Seq.max

let buildInitialEvents cargo =
    let initialCargo = cargo |> List.mapi (fun i c -> CargoReadyForDelivery(CargoIdentifier.Identifier i, c))

    let initialVehicles =
        [ VehicleProvided(Truck T1, Factory)
          VehicleProvided(Truck T2, Factory)
          VehicleProvided(Ship, Port) ]

    List.concat [ initialCargo; initialVehicles ] |> List.map (fun e -> e, 0)

module Program =

    let parseInput (input: string []) =
        match input with
        | [| singleArgument |] ->
            singleArgument.ToCharArray()
            |> Seq.map (function
                | 'A' -> A
                | 'B' -> B
                | x -> failwithf "Unknown cargo %c" x)
        | x -> failwithf "Expecting a single string containing the cargo list but got %A" x

    [<EntryPoint>]
    let main argv =
        printfn "%A" argv
        let cargo = parseInput argv |> Seq.toList

        let events =
            cargo
            |> buildInitialEvents
            |> iterate
        let highest =
            events
            |> findLatestDelivery

        printfn "Highest time %A" highest
        printfn "%A" events

        0 // return an integer exit code
