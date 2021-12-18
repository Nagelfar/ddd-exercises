import domain.{Actor, ArriveAt, Cargo, Location, LocationId, Ship, StoreCargo, Tick, TickMessage, Truck, Vehicle, VehicleId}

object World {
  def main(args: Array[String]): Unit = {

    val p = World()
    p.moveCargoTo(p.warehouseA.id)
    p.moveCargoTo(p.warehouseA.id)

    p.startSimulation()
  }
}

case class World() {
  val factory: Location = Location(LocationId("FACTORY"))
  val port: Location = Location(LocationId("PORT"))
  val warehouseA: Location = Location(LocationId("A"))
  val warehouseB: Location = Location(LocationId("B"))

  val truck1: Truck = Truck(VehicleId("1"))
  val truck2: Truck = Truck(VehicleId("2"))
  val ship: Ship = Ship(VehicleId("3"))

  val locations = Seq(factory, warehouseA, warehouseB)
  val vehicles = Seq(truck1, truck2, ship)
  val actors: Map[domain.ActorRef, Actor] = List.concat(locations, vehicles).map(a => (a.id, a)).toMap

  var cargoId = 0
  var time = 0

  private def nextCargoId() = {
    cargoId = cargoId + 1
    cargoId.toString
  }

  def moveCargoTo(destination: LocationId) = {
    val cargo = Cargo(nextCargoId(), destination, factory.id)
    factory.receive(StoreCargo(factory.id, cargo))
  }

  def startSimulation() = {
    truck1.receive(ArriveAt(factory.id, time))
    truck2.receive(ArriveAt(factory.id, time))
    ship.receive(ArriveAt(port.id, time))

    tick(time)
    time = time + 1
    tick(time)
    time = time + 1
    tick(time)

  }

  def tick(time: Tick) = {
    actors.values.foreach(v => v.receive(TickMessage(time)))

    var hasMessages = true
    do {
      val messages = actors.values.flatMap(a => a.publishedMessages()).toList
      messages.foreach(message => println(s"Received message ${message._2} for Actor ${message._1}"))

      messages.foreach(message => actors.get(message._1).foreach(a => a.receive(message._2)))
      hasMessages = messages.nonEmpty
    } while (hasMessages)
  }
}

