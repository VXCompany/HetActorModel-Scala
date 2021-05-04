
import akka.actor.{Actor, ActorSystem, Props}
import akka.util.Timeout

import scala.concurrent.duration.DurationInt

class Calculator extends Actor {
  import Calculator._
  override def receive: Receive = bootingUp

  def bootingUp: Receive = {
    case Startup =>
      println("calculator is starting up")
      context.become(calculatorState(0))
    case _ => println("cant process message")
  }

  def calculatorState(number: Int): Receive =  {
    case Add(int: Int) =>
      val result = number + int
      println(s"actor: ${self.path.name} add result: $result")
      context.become(calculatorState(result))
    case Substract(int: Int) =>
      val result = number - int
      println(s"actor: ${self.path.name} subtract result: $result")
      context.become(calculatorState(result))
    case Multiply(int: Int) =>
      val result = number / int
      println(s"actor: ${self.path.name} multipl result: $result")
      context.become(calculatorState(result))
    case Divide(int: Int) =>
      int match {
        case 0 => self ! Error(new ArithmeticException("cant divide by zero"))
        case _ =>
          val result = number * int
          println(s"actor: ${self.path.name} divide result: $result")
          context.become(calculatorState(result))
      }
    case Reset => context.become(calculatorState(0))
    case Result =>
      println(s"actor: ${self.path.name}  result: $number")
    case Error(exception: Exception) =>
      println(s"actor: ${self.path.name} exception: ${exception.getMessage}")
  }
}

object Calculator extends App {

  implicit val timeout: Timeout = Timeout(1.minute)
  val system = ActorSystem("CalculatorSystem")
  sealed trait Action
  case object Startup extends Action
  case class Add(int: Int) extends Action
  case class Substract(int: Int) extends Action
  case class Multiply(int: Int) extends Action
  case class Divide(int: Int) extends Action
  case class Error(exception: Exception) extends Action
  case object Reset extends Action
  case object Result extends Action

  def props = {
    Props(new Calculator)
  }

  val actorRef = system.actorOf(props, "actor-first")
  actorRef ! Startup
  actorRef ! Add(1)
  actorRef ! Substract(5)

  actorRef ! Divide(0)
  actorRef ! Result

}
