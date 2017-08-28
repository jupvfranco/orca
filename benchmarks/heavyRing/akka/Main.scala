import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import akka.util.Timeout
import akka.pattern.ask
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import com.typesafe.config.ConfigValueFactory
import com.typesafe.config.impl.ConfigLong
import scala.language.postfixOps
import scala.concurrent.duration._
 
final class Tree(left:Tree, right:Tree) {
  def count:Int = if (left eq null) 1 else 1 + left.count + right.count
}
 
object Tree {
  def apply(depth:Int):Tree = {
    if (depth > 1) new Tree(Tree(depth-1), Tree(depth-1))
    else new Tree(null, null)
  }
}
 
case class Token(n:Int, t:Tree)
 
class ThreadRingActor(id : Int, var next: ActorRef) extends Actor {
  def receive: Actor.Receive = {
    case Token(n:Int, t:Tree) => {
      if (n == 0) {
        println(id)
        context.actorSelection("/user/reaper") ! "done"
      } else
        next.tell(Token(n-1, t), null);
    }
    case n: ActorRef => { this.next = n }
  }
}
 
class Reaper(var countdown: Int) extends Actor {
  def receive: Actor.Receive = {
    case "done" => {
      countdown -= 1
      if (countdown == 0) {
        System.exit(0)
      }
    }
  }
}
 
object Main {
  // val config =
  //   ConfigFactory.parseString(
  //       """
  //       akka{actor{default-dispatcher{fork-join-executor{parallelism-max = 1}}}}
  //       """
  //   )
  //   .withFallback(ConfigFactory.defaultReference(this.getClass.getClassLoader))
 
  // val system = ActorSystem("system",config)
 
  val system = ActorSystem("system")
 
  implicit val timeout = Timeout(5 seconds)
  implicit val ec = system.dispatcher
 
  var n_actors:Int = 0
  var depth:Int = 0
  var n_rounds:Int = 0
  var n_messages:Int = 0
 
  def setup_ring(id: Int) = {
    var actor_index = n_actors
    var actor_id = n_actors * id + actor_index
    val last = system.actorOf(
          Props(classOf[ThreadRingActor],actor_index,null),
          (actor_id).toString()
        )
    var current:ActorRef = null
    var next:ActorRef = last
    for (i <- n_actors-1 to 1 by -1) {
      actor_index = i
      actor_id = n_actors * id + actor_index
      current = system.actorOf(
          Props(classOf[ThreadRingActor],actor_index,next),
          (actor_id).toString()
        )
      next = current
    }
    (last ? next) onComplete {
      case _ => next ! Token(n_messages, Tree(depth))
    }
  }
 
  def main(args: Array[String]) {
    if (args.size < 3) {
      println("program <#actors> <#depth> <#rounds>")
      System.exit(-1)
    }
    n_actors = args(0).toInt
    depth = args(1).toInt
    n_rounds = args(2).toInt
    n_messages = n_actors * n_rounds
 
    system.actorOf(Props(classOf[Reaper], 1), "reaper")
    setup_ring(1)
  }
}

