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

case class Request(id:Int, depth:Int, next:ActorRef, start:Long)
case class Response(id:Int, count:Int, start:Long)

class Final(var n_requests:Int) extends Actor {
  def receive: Actor.Receive = {
    case Response(id, count, start) => {
      val end = System.nanoTime()
      println(s"($id, $count, ${(end-start)/1000})")
      n_requests -= 1
      if (n_requests == 0) {
        context.actorSelection("/user/reaper") ! "done"
      }
    }
  }
}

class Server() extends Actor {
  def receive: Actor.Receive = {
    case Request(id, depth, next, start) =>
      next ! Response(id, Tree(depth).count, start)
  }
}

class Client(id:Int, server:ActorRef, n_requests:Int, next:ActorRef)
    extends Actor {

  override
  def preStart = {
    for (i <- 0 until n_requests) {
      val depth = i*2 + 6
      val start = System.nanoTime()
      server ! Request(id, depth, next, start)
    }
  }

  def receive: Actor.Receive = {
    case _ => println("sth")
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

  def main(args: Array[String]) {
    if (args.size < 3) {
      println("program <#server> <#client> <#requests>")
      System.exit(-1)
    }
    val n_servers = args(0).toInt
    val n_clients = args(1).toInt
    val n_requests = args(2).toInt

    val servers = (
        for (i <- 0 until n_servers)
          yield system.actorOf(Props(classOf[Server]), "server_" + i)
        )

    system.actorOf(Props(classOf[Reaper], n_clients), "reaper")

    for (i <- 0 until n_clients) {
      val next = system.actorOf(Props(classOf[Final], n_requests), "final_" + i)
      system.actorOf(Props(classOf[Client],
            i, servers(i%n_servers), n_requests, next), "client_" + i)
    }
  }
}
