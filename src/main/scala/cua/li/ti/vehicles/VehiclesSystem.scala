package cua.li.ti.vehicles
import akka.actor.{Actor,ActorLogging,ActorRef,ActorSystem,Props}
import scala.collection._
import scala.concurrent.Future
import scala.util.Random
object VehiclesSystem {
  def main(args :Array[String]) :Unit = {
    val system = ActorSystem("vehicles")
    system.actorOf(Props[Satellite], "gps")
    val enterprises = for (index <- 1 to 42)
      yield system.actorOf(Props[Enterprise], "enterprise_"+index)
    for (enterprise <- enterprises) {
      enterprise ! Protocol.Buy
    }
  }
}
object Protocol {
  case object Buy
  case class Operate(enterprise :String, plate :String)
  case class Heartbeat(vehicle :ActorRef)
  case class Position(lat :Double, lng :Double)
}
class Enterprise extends Actor {
  import Protocol._
  override def receive = {
  	case Buy => {
  	  if (Random.nextBoolean) self ! Buy
  	  val vehicle = context.actorOf(Props[Vehicle])
  	  vehicle ! Operate(self.path.name, plate)
  	}
  }
  private def plate = {
  	val sb = StringBuilder.newBuilder
  	for (letters <- 0 until 3)
  	  sb += ('A'.toInt + Random.nextInt(26)).toChar
  	sb += '-'
  	for (numbers <- 0 until 4)
  	  sb += ('0'.toInt + Random.nextInt(10)).toChar
  	sb.toString
  }
}
class Satellite extends Actor {
  import context.dispatcher
  import Protocol._
  override def receive = {
  	case Heartbeat(vehicle) => {
  	  val future = Future {
  	  	Thread.sleep(9000+(Random.nextGaussian * 2000).toInt)
  	  	self ! Heartbeat(vehicle)
  	  }
  	  vehicle ! Position(Random.nextDouble, Random.nextDouble)
  	}
  }
}
class Vehicle extends Actor with ActorLogging {
  import Protocol._
  var plate = "no plate"
  var enterprise = "no owner"
  override def receive = {
    case Operate(owner, license) => {
      enterprise = owner
      plate = license
      context.system.actorSelection("/user/gps") ! Heartbeat(self)
    }
  	case Position(lat, lng) =>
  	  log.info(s"in $enterprise, $plate at ($lat,$lng)")
  }
}
// That's all folks!