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

type Event =
    | CargoReadyForDelivery of Cargo
    | DepartingLocation of Vehicle * Location * Cargo
    | ArrivedBack of Vehicle * Location
    | ParkedShipment of Cargo * Location
    | PickedUpShipment of Cargo * Location
    | DeliveredShipment of Cargo

type Entry = Event * Timepoint

type State =
    { TrucksAtFactory: Map<Truck, Timepoint>
      ShipWaitingAtPort: Timepoint
      CargoAtFactory: Cargo list
      CargoWaitingForPickupAtPort: Map<Cargo, Timepoint>
      CurrentTime: Timepoint
      CargoDelivered: (Cargo * Timepoint) list }

let update (state: State) event =
    match event with
    | CargoReadyForDelivery c, _ -> { state with CargoAtFactory = state.CargoAtFactory @ [ c ] }
    | DepartingLocation(Truck t, Factory, _), _ -> { state with TrucksAtFactory = state.TrucksAtFactory.Remove t }
    | ArrivedBack(Truck t, Factory), time -> { state with TrucksAtFactory = state.TrucksAtFactory.Add(t, time) }
    | ArrivedBack(Ship, Port), time -> { state with ShipWaitingAtPort = time }
    | ParkedShipment(c, Port), time ->
        { state with CargoWaitingForPickupAtPort = state.CargoWaitingForPickupAtPort.Add(c, time) }
    | DeliveredShipment(c), time -> { state with CargoDelivered = state.CargoDelivered @ [ (c, time) ] }
    | PickedUpShipment(c, Port), _ ->
        { state with CargoWaitingForPickupAtPort = state.CargoWaitingForPickupAtPort.Remove c }
    | PickedUpShipment(c, Factory), _ -> { state with CargoAtFactory = state.CargoAtFactory.Tail }
    | DeliveredShipment _, _ | ArrivedBack(_), _ | ParkedShipment _, _ | PickedUpShipment _, _ | DepartingLocation _, _ 
        -> state

let pickUpCargoAtFactory time cargo truck =
    match cargo with
    | (_, A) ->
        [ Entry(PickedUpShipment(cargo, Factory), time)
          Entry(DepartingLocation(Truck truck, Factory, cargo), time)
          Entry(ParkedShipment(cargo, Port), time + 1)
          Entry(ArrivedBack(Truck truck, Factory), time + 2) ]
    | (_, B) ->
        [ Entry(PickedUpShipment(cargo, Factory), time)
          Entry(DepartingLocation(Truck truck, Factory, cargo), time)
          Entry(DeliveredShipment(cargo), time + 5)
          Entry(ArrivedBack(Truck truck, Factory), time + 10) ]

let pickUpCargoAtPort time cargo ship =
    [ Entry(PickedUpShipment(cargo, Port), time)
      Entry(DepartingLocation(ship, Port, cargo), time)
      Entry(DeliveredShipment(cargo), time + 4)
      Entry(ArrivedBack(ship, Port), time + 8) ]

let moveCargoFromFactory state =
    let avaliableCargo = List.tryHead state.CargoAtFactory

    let avaliableTruck =
        state.TrucksAtFactory
        |> Map.filter (fun _ v -> v <= state.CurrentTime)
        |> Map.toSeq
        |> Seq.map fst
        |> Seq.tryHead

    Option.map2 (pickUpCargoAtFactory state.CurrentTime) avaliableCargo avaliableTruck

let firstCargoAtPort state =
    state.CargoWaitingForPickupAtPort
    |> Map.filter (fun _ v -> v <= state.CurrentTime)
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.tryHead

let avaliableShip state =
    if state.ShipWaitingAtPort <= state.CurrentTime then Some Ship
    else None

let moveCargoFromPort state =
    let cargoAtPort = firstCargoAtPort state
    let ship = avaliableShip state
    Option.map2 (pickUpCargoAtPort state.CurrentTime) cargoAtPort ship

let step state =
    let mutable mState =
        moveCargoFromPort state
        |> Option.toList
        |> List.collect id
        |> List.fold update state

    let mutable eventsFromFactory = moveCargoFromFactory mState
    while eventsFromFactory.IsSome do
        let newState = eventsFromFactory.Value |> Seq.fold update mState
        eventsFromFactory <- moveCargoFromFactory newState
        mState <- newState

    mState

let iterate (initial: State) =
    let mutable state = initial
    while state.CargoAtFactory.Length > 0 || (firstCargoAtPort state).IsSome do
        let stepState = step state
        state <- { stepState with CurrentTime = stepState.CurrentTime + 1 }
    state

let findLatestDelivery (state: State) =
    state.CargoDelivered
    |> Seq.maxBy (fun (_, t) -> t)
    |> snd

let buildInitialState cargo =
    let initialCargo = cargo |> List.mapi (fun i c -> CargoReadyForDelivery(Identifier i, c), 0)

    let initialVehicles =
        [ ArrivedBack(Truck T1, Factory), 0
          ArrivedBack(Truck T2, Factory), 0
          ArrivedBack(Ship, Port), 0 ]

    let initialState =
        { TrucksAtFactory = Map.empty
          CargoAtFactory = []
          CargoDelivered = []
          CurrentTime = 0
          ShipWaitingAtPort = 0
          CargoWaitingForPickupAtPort = Map.empty }

    Seq.concat [ initialCargo; initialVehicles ] |> Seq.fold update initialState

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
            |> buildInitialState
            |> iterate
            |> findLatestDelivery

        printfn "Highest time %A" highest

        0 // return an integer exit code
