package domain

import scala.collection.mutable
import scala.collection.mutable.ListBuffer

sealed trait Message

case class TickMessage(now: Tick) extends Message

case class StoreCargo(location: LocationId, cargo: Cargo) extends Message

case class PossibleRoute(from: LocationId, to: LocationId, targeting: LocationId, durationInTicks: Int) extends Message

case class ArriveAt(location: LocationId, now: Tick) extends Message

case class ReadyForPickup(locationId: LocationId, vehicleId: VehicleId, capacity: Int) extends Message

case class MoveCargo(id: domain.VehicleId, cargo: Seq[domain.Cargo]) extends Message

trait Actor {
  val id: ActorRef

  private var messages: Seq[(ActorRef, Message)] = Seq()

  def tell(id: ActorRef, message: Message) = {
    messages = Seq.concat(messages, Seq((id, message)))
  }

  def publishedMessages() = {
    val published = messages
    messages = Seq()
    published
  }

  def receive(message: Message)
}


case class Location(id: LocationId) extends Actor {
  private val cargo: mutable.Queue[Cargo] = mutable.Queue()

  override def receive(message: Message) = {
    message match {
      case TickMessage(now) =>
      case StoreCargo(location, c) if location == id =>
        cargo.append(c)
      case ReadyForPickup(l, v, capacity) if l == id =>
        val cargoToTransport: ListBuffer[Cargo] = mutable.ListBuffer()
        var next = cargo.nonEmpty
        while (next) {
          if (cargoToTransport.nonEmpty && cargoToTransport.head.destination != cargo.head.destination)
            next = false
          else {
            cargoToTransport.addOne(cargo.dequeue())
            next = cargo.nonEmpty
          }
        }

        if (cargoToTransport.nonEmpty)
          tell(v, MoveCargo(v, cargoToTransport.toSeq))
      case _ =>
    }
  }
}

abstract class Vehicle(id: VehicleId, transportCapacity: Int) extends Actor {
  var positionAt: Option[(LocationId, Tick)] = None
  var cargoOnBoard: Option[Seq[Cargo]] = None
  var destination = Option[LocationId] = None

  var routes: Map[LocationId, PossibleRoute] = Map()

  override def receive(message: Message) = {
    message match {
      case PossibleRoute(from, to, targeting, duration) as r =>
        routes = routes.updated(targeting, r)
      case TickMessage(now) =>
        positionAt match {
          case Some((l, t)) if t == now =>
            tell(l, ReadyForPickup(l, id, transportCapacity))
          case _ =>
        }
      case ArriveAt(locationId, time) =>
        positionAt = Some((locationId, time))
      case MoveCargo(v, cargo) if v == id =>
        cargoOnBoard = Some(cargo)
        destination = Some(cargo.head.destination)
      case _ =>
    }
  }
}

case class Truck(id: VehicleId) extends Vehicle(id, 1)

case class Ship(id: VehicleId) extends Vehicle(id, 4)