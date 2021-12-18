package object domain {

  trait ActorRef extends java.io.Serializable {
    val id: String
    val actorRef = this.getClass + "/" + id
  }

  case class LocationId(id: String) extends ActorRef

  case class VehicleId(id: String) extends ActorRef

  type Tick = Int

  case class Cargo(id: String, destination: LocationId, origin: LocationId)

}
