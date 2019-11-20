module Exercise2

type Destination = A | B
type CargoIdentifier = Identifier of int

type Cargo = CargoIdentifier * Destination

type Location =
    | Factory
    | Port
    | Warehouse of Destination

type Truck = T1| T2
type Vehicle =
    | Truck of Truck
    | Ship

type Timepoint = int

type TransportIdentifier = Identifier of int

type Event =
    | CargoReadyForDelivery of Cargo
    | VehicleProvided of Vehicle * Location
    | TransportCreated of TransportIdentifier
    | Departing of TransportIdentifier * Vehicle * from: Location * Cargo * destination:Location
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
              Entry(Departing(transportId,Truck truck, Factory, cargo, Port), time)
              Entry(ParkedShipment(cargo, Port), time + 1)
              Entry(ArrivedBack(transportId,Truck truck, Factory), time + 2) ]
        | (_, B) ->
            [ Entry(TransportCreated(transportId), time)
              Entry(PickedUpShipment(cargo, Factory), time)
              Entry(Departing(transportId,Truck truck, Factory, cargo, Warehouse B), time)
              Entry(DeliveredShipment(cargo), time + 5)
              Entry(ArrivedBack(transportId,Truck truck, Factory), time + 10) ]

    let pickUpCargoAtPort time transportId cargo ship =
        [ Entry(TransportCreated(transportId), time)
          Entry(PickedUpShipment(cargo, Port), time)
          Entry(Departing(transportId, ship, Port, cargo, Warehouse A), time)
          Entry(DeliveredShipment(cargo), time + 4)
          Entry(ArrivedBack(transportId, ship, Port), time + 8) ]

type State =
    { TrucksAtFactory: Map<Truck, Timepoint>
      ShipWaitingAtPort: Timepoint
      CargoAtFactory: Cargo list
      CargoWaitingForPickupAtPort: Map<Cargo, Timepoint>
      CargoDelivered: (Cargo * Timepoint) list
      NextTransportId: TransportIdentifier }
      with static member Initial = 
            { TrucksAtFactory = Map.empty
              CargoAtFactory = []
              CargoDelivered = []
              ShipWaitingAtPort = 0
              CargoWaitingForPickupAtPort = Map.empty
              NextTransportId = Identifier(0) }

let update (state: State) event =
    match event with
    | TransportCreated(Identifier(id)),_ -> {state with NextTransportId = Identifier(id + 1)}
    | CargoReadyForDelivery c, _ -> { state with CargoAtFactory = state.CargoAtFactory @ [ c ] }
    | Departing(_,Truck t, Factory, _,_), _ -> { state with TrucksAtFactory = state.TrucksAtFactory.Remove t }
    | ArrivedBack(_,Truck t, Factory), time 
    | VehicleProvided(Truck t, Factory), time -> { state with TrucksAtFactory = state.TrucksAtFactory.Add(t, time) }
    | ArrivedBack(_,Ship, Port), time 
    | VehicleProvided(Ship, Port), time -> { state with ShipWaitingAtPort = time }
    | Arrived(_),_ -> state
    | ParkedShipment(c, Port), time ->
        { state with CargoWaitingForPickupAtPort = state.CargoWaitingForPickupAtPort.Add(c, time) }
    | DeliveredShipment(c), time -> { state with CargoDelivered = state.CargoDelivered @ [ (c, time) ] }
    | PickedUpShipment(c, Port), _ ->
        { state with CargoWaitingForPickupAtPort = state.CargoWaitingForPickupAtPort.Remove c }
    | PickedUpShipment(c, Factory), _ -> { state with CargoAtFactory = state.CargoAtFactory.Tail }
    | DeliveredShipment _, _ | ArrivedBack(_), _ | ParkedShipment _, _ | PickedUpShipment _, _ | Departing _, _ | VehicleProvided _,_ 
        -> state

let buildState events =
    events
    |> Seq.fold update State.Initial

let findFirstAvaliable items time =
    items
    |> Map.filter (fun _ v -> v <= time)
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.tryHead

let avaliableShip time state =
    if state.ShipWaitingAtPort <= time then Some Ship
    else None

let moveCargoFromPort time state =
    let cargo = findFirstAvaliable state.CargoWaitingForPickupAtPort time
    let vehicle = avaliableShip time state
    Option.map2 (Domain.pickUpCargoAtPort time state.NextTransportId) cargo vehicle
    |> Option.defaultValue []

let moveCargoFromFactory time state =
    let cargo = List.tryHead state.CargoAtFactory
    let vehicle = findFirstAvaliable state.TrucksAtFactory time
    Option.map2 (Domain.pickUpCargoAtFactory time state.NextTransportId) cargo vehicle
    |> Option.defaultValue []

let isCargoWaitingOnPort time state = 
    (findFirstAvaliable state.CargoWaitingForPickupAtPort time).IsSome

let step time events =
    let eventsFromPort = moveCargoFromPort time (buildState events)
    let mutable mEvents = events @ eventsFromPort
    let mutable tryMoveCargo = true
    while tryMoveCargo do
        let eventsFromFactory = moveCargoFromFactory time (buildState mEvents)
        tryMoveCargo <- not eventsFromFactory.IsEmpty
        mEvents <- mEvents @ eventsFromFactory
        
    mEvents

let iterate intialEvents =
    let mutable events: Entry list = intialEvents
    let mutable notFinished = true
    let mutable time = 0
    while notFinished do
        events <- step time events
        time <- time + 1
        let state = buildState events
        notFinished <- state.CargoAtFactory.Length > 0 || (isCargoWaitingOnPort time state)
    events

let findLatestDelivery events =
    let state = events |> buildState
    state.CargoDelivered
    |> Seq.maxBy (fun (_, t) -> t)
    |> snd

let buildInitialEvents cargo =
    let initialCargo = cargo |> List.mapi (fun i c -> CargoReadyForDelivery(CargoIdentifier.Identifier i, c), 0)

    let initialVehicles =
        [ VehicleProvided(Truck T1, Factory), 0
          VehicleProvided(Truck T2, Factory), 0
          VehicleProvided(Ship, Port), 0 ]

    List.concat [ initialCargo; initialVehicles ]

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

        let highest =
            cargo
            |> buildInitialEvents
            |> iterate
            |> findLatestDelivery

        printfn "Highest time %A" highest

        0 // return an integer exit code
