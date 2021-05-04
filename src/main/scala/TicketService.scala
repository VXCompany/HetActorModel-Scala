import akka.actor.{Actor, ActorSystem, Props}

class TicketService extends Actor {
  import TicketService._
  override def receive: Receive = preSale

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
        context.become(sale(newTotal))
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


}
