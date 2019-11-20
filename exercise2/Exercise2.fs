module Exercise2

type Destination = A | B

type CargoIdentifier = Identifier of int
type Cargo =  CargoIdentifier * Destination
type Location =
    | Factory
    | Port
    | Warehouse of Destination

type Truck = T1 | T2
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
    { TrucksAtFactory: Map<Timepoint, Truck Set>
      ShipWaitingAtPort: Timepoint
      CargoAtFactory: Cargo list
      CargoWaitingForPickupAtPort: Map<Cargo, Timepoint>
      CurrentTime: Timepoint
      CargoDelivered: (Cargo * Timepoint) list }
      
let update (state: State) event =
    match event with
    | CargoReadyForDelivery c, _ -> { state with CargoAtFactory = state.CargoAtFactory @ [c] }
    | DepartingLocation (Truck t, Factory, _), time -> 
        let entry = Set.remove t state.TrucksAtFactory.[time]
        {state with TrucksAtFactory = Map.add time entry state.TrucksAtFactory }
    | ArrivedBack(Truck t, Factory), time when state.TrucksAtFactory.ContainsKey time -> 
        let entry = Set.add t state.TrucksAtFactory.[time]
        { state with TrucksAtFactory = Map.add time entry state.TrucksAtFactory}
    | ArrivedBack(Truck t, Factory), time -> 
        {state with TrucksAtFactory = state.TrucksAtFactory.Add(time, set [ t ]) }
    | ArrivedBack(Ship, Port), time -> { state with ShipWaitingAtPort = time }
    | ParkedShipment(c, Port), time -> {state with CargoWaitingForPickupAtPort = state.CargoWaitingForPickupAtPort.Add (c, time)}     
    | DeliveredShipment (c), time -> {state with CargoDelivered = state.CargoDelivered @ [(c, time)]}
    | PickedUpShipment(c , Port), _ -> {state with CargoWaitingForPickupAtPort = state.CargoWaitingForPickupAtPort.Remove c }
    | PickedUpShipment(c, Factory),_ -> {state with CargoAtFactory = state.CargoAtFactory.Tail }
    | DeliveredShipment _, _ | ArrivedBack(_), _  | ParkedShipment _ ,_ | PickedUpShipment _, _ | DepartingLocation _,_ -> state

let pickUpCargoAtFactory cargo truck time =
    match cargo with
    | (_, A) as c->
        [ 
            Entry(PickedUpShipment(c, Factory), time)
            Entry(DepartingLocation(Truck truck, Factory, c), time)
            Entry(ParkedShipment(c,Port), time + 1)
            Entry(ArrivedBack(Truck truck, Factory), time + 2) 
        ]
    | (_,B) as c ->
        [ 
            Entry(PickedUpShipment(c, Factory), time)
            Entry(DepartingLocation(Truck truck, Factory, c), time)
            Entry(DeliveredShipment (c), time + 5)
            Entry(ArrivedBack(Truck truck, Factory), time + 10) 
        ]    

let moveCargoFromFactory state = 
    let cargoAvaliable = 
        List.tryHead state.CargoAtFactory
    let trucksAvaliable = 
        state.TrucksAtFactory
        |> Map.tryFind state.CurrentTime
        |> Option.map Set.toSeq
        |> Option.bind Seq.tryHead
        |> Option.map2 (fun c t -> pickUpCargoAtFactory c t state.CurrentTime) cargoAvaliable

    trucksAvaliable

let firstCargoAtPort state = 
    state.CargoWaitingForPickupAtPort
    |> Map.filter (fun _ v -> v <= state.CurrentTime)
    |> Map.toSeq
    |> Seq.map fst
    |> Seq.tryHead    

let moveCargoFromPort state =
    let cargoAtPort = firstCargoAtPort state
    if state.ShipWaitingAtPort <= state.CurrentTime && cargoAtPort.IsSome then   
        let cargo = cargoAtPort.Value
        [ 
            Entry(PickedUpShipment(cargo, Port), state.CurrentTime)
            Entry(DepartingLocation(Ship,Port, cargo), state.CurrentTime)
            Entry(DeliveredShipment (cargo), state.CurrentTime + 4)
            Entry(ArrivedBack(Ship, Port), state.CurrentTime + 8) 
        ]
    else []

let step state =
    let ts = 
        moveCargoFromPort state
        |> Seq.fold update state    

    let mutable mState = ts
    let mutable eventsFromFactory = moveCargoFromFactory ts
    while eventsFromFactory.IsSome do
        let newState = 
            eventsFromFactory.Value       
            |> Seq.fold update mState
        eventsFromFactory <- moveCargoFromFactory newState 
        mState <- newState

    mState       

let iterate (initial: State) =
    let mutable state = initial
    while state.CargoAtFactory.Length > 0 || (firstCargoAtPort state).IsSome do
        let stepState = step state
        state <- {stepState with CurrentTime = stepState.CurrentTime + 1 }
    state    

let findLatestDelivery (state: State) = 
    state.CargoDelivered 
    |> Seq.maxBy (fun (_, t) -> t)
    |> snd

let buildInitialState cargo =
    let initialCargo =
        cargo
        |> List.mapi(fun i c -> CargoReadyForDelivery (Identifier i, c), 0)

    let initialTrucks = 
        [ 
            ArrivedBack(Truck T1,Factory), 0
            ArrivedBack(Truck T2,Factory), 0
            ArrivedBack(Ship, Port), 0 
        ]

    let initialState =
        { TrucksAtFactory = Map.empty
          CargoAtFactory = []
          CargoDelivered = []
          CurrentTime = 0          
          ShipWaitingAtPort = 0
          CargoWaitingForPickupAtPort = Map.empty
          }

    Seq.concat [initialCargo ; initialTrucks]
    |> Seq.fold update initialState 

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
        let cargo = 
            parseInput argv
            |> Seq.toList

        let highest =
            cargo
            |> buildInitialState
            |> iterate
            |> findLatestDelivery

        printfn "Highest time %A" highest

        0 // return an integer exit code
