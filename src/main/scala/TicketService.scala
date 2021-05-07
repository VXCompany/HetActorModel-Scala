import java.util.concurrent.TimeUnit

import akka.actor.{Actor, ActorRef, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration.FiniteDuration

class TicketService extends Actor {
  import TicketService._
  override def receive: Receive = preSale

  //Database
  var boughtTickets: List[String] = List()

  def preSale: Receive = {
    case Init(totalAmountOfTickets) =>
      context.become(sale(totalAmountOfTickets))
      self ! StartSelling
    case _ => println("Ticket sale not started")
  }

  def sale(totalAmountOfTickets: Int): Receive = {
    case StartSelling => println(s"Sale is started with amount of tickets: $totalAmountOfTickets")
    case Buy(actorRef, numberOfTickets) =>
      println(s"seller do you have $numberOfTickets tickets for sell?")
      val newTotal = totalAmountOfTickets - numberOfTickets
      if (newTotal >= 0) {
        println(s"successfully purchased ticket, $totalAmountOfTickets, dont forget negative covid test $actorRef")
        boughtTickets = boughtTickets :+ actorRef.toString
        actorRef ! Bought(numberOfTickets)
        context.become(sale(newTotal))
      } else {
        actorRef ! SoldOut(numberOfTickets)
        context.become(postSale("sold out"))
      }
    case PositiveCovidTest(actorRef, numberOfTickets) => ticketReturn(actorRef, numberOfTickets)
  }

  def postSale(reason: String): Receive = {
    case PositiveCovidTest(actorRef, numberOfTickets) => ticketReturn(actorRef, numberOfTickets)
    case _ => println(s"sale already ended $reason")
  }

  def ticketReturn(actorRef: ActorRef, numberOfTickets: Int) = {
    if (boughtTickets.contains(actorRef.toString())) {
      boughtTickets = boughtTickets.filterNot(_.equals(actorRef.toString()))
      println(s"returned amount: $numberOfTickets of tickets")
      context.become(sale(numberOfTickets))
    } else {
      println("you dont have any tickets")
    }
  }
}

class TicketBuyer extends Actor {
  import TicketService._
  implicit val timeout: Timeout = Timeout(FiniteDuration(1, TimeUnit.SECONDS))
  override def receive: Receive = handleTickets

  def handleTickets: Receive = {
    case Bought(numberOfTickets) => println(s"${self.path} succesfully bought $numberOfTickets")
    case SoldOut(numberOfTickets) => println(s"${self.path} tickets already sold out")
    case Returned(numberOfTickets) => println(s"${self.path} succesfully returned $numberOfTickets")
  }
}

object TicketService extends App {
  val system = ActorSystem("TicketService")
  sealed trait TicketSellerCommand
  case class Init(totalAmountOfTickets: Int) extends TicketSellerCommand
  case class Buy(actorRef: ActorRef, numberOfTickets: Int) extends TicketSellerCommand
  case class PositiveCovidTest(actorRef: ActorRef, numberOfTickets: Int) extends TicketSellerCommand
  case object StartSelling


  sealed trait TicketBuyerCommand
  case class Bought(numberOfTickets: Int) extends TicketBuyerCommand
  case class SoldOut(numberOfTickets: Int) extends TicketBuyerCommand
  case class Returned(numberOfTickets: Int) extends TicketBuyerCommand

  val actorRefTicketService = system.actorOf(Props(new TicketService), "ticketservice")

  actorRefTicketService ! Init(2)

  val actorRefTicketBuyer1 = system.actorOf(Props(new TicketBuyer), "ticketBuyer1")
  val actorRefTicketBuyer2 = system.actorOf(Props(new TicketBuyer), "ticketBuyer2")
  val actorRefTicketBuyer3 = system.actorOf(Props(new TicketBuyer), "ticketBuyer3")




  actorRefTicketService ! Buy(actorRefTicketBuyer1, 2)
  actorRefTicketService ! Buy(actorRefTicketBuyer2, 2)
  actorRefTicketService ! Buy(actorRefTicketBuyer3, 2)
  actorRefTicketService ! PositiveCovidTest(actorRefTicketBuyer1, 2)
  actorRefTicketService ! Buy(actorRefTicketBuyer2, 2)


}
