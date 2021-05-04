import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.FiniteDuration
import scala.util.{Failure, Success}

class TicketService extends Actor {
  import TicketService._
  override def receive: Receive = preSale

  var boughtTickets: List[String] = List()

  def preSale: Receive = {
    case Init(totalAmountOfTickets) =>
      context.become(sale(totalAmountOfTickets))
      self ! StartSelling
    case _ => println("Ticket sale not started")
  }

  def sale(totalAmountOfTickets: Int): Receive = {
    case StartSelling => println(s"Sale is started with amount of tickets: $totalAmountOfTickets")
    case Buy(numberOfTickets) =>
      val newTotal = totalAmountOfTickets - numberOfTickets
      if (newTotal >= 0) {
        val actorRef = sender()
        println(s"successfully purchased ticket, dont forget negative covid test $actorRef")
        boughtTickets = boughtTickets :+ actorRef.toString
        context.become(sale(newTotal))
      }
  }

}

class TicketBuyer extends Actor {
  import TicketService._
  implicit val timeout: Timeout = Timeout(FiniteDuration(1, TimeUnit.SECONDS))
  override def receive: Receive = handleTickets

  def handleTickets: Receive = {
    case Buy(numberOfTickets) => sendCommandToActorRefTicketSystem(Buy(numberOfTickets))
  }

  def sendCommandToActorRefTicketSystem(ticketBuyerCommand: TicketBuyerCommand): Unit = {
    system.actorSelection("user/ticketservice").resolveOne().onComplete {
      case Success(actorRef: ActorRef) => actorRef ! ticketBuyerCommand
      case Failure(exception) =>
        println("ticket system not found")
        throw exception
    }
  }
}


object TicketService extends App {
  val system = ActorSystem("TicketService")
  sealed trait TicketSellerCommand
  case class Init(totalAmountOfTickets: Int) extends TicketSellerCommand
  case object StartSelling


  sealed trait TicketBuyerCommand
  case class Buy(numberOfTickets: Int) extends TicketBuyerCommand


  val actorRefTicketService = system.actorOf(Props(new TicketService), "ticketservice")

  actorRefTicketService ! Init(2)

  val actorRefTicketBuyer1 = system.actorOf(Props(new TicketBuyer), "ticketBuyer1")
  actorRefTicketBuyer1 ! Buy(2)

  val actorRefTicketBuyer2 = system.actorOf(Props(new TicketBuyer), "ticketBuyer2")
  actorRefTicketBuyer2 ! Buy(2)



}
