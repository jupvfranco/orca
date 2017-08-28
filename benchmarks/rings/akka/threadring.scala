/* The Computer Language Benchmarks Game
   http://benchmarksgame.alioth.debian.org/

   contributed by Stefan Ettrup

*/
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

class ThreadRingActor(id : Int, var next: ActorRef) extends Actor {
  def receive: Actor.Receive = {
    case e: Int => {
      if (e == 0) {
        println(id)
        context.actorSelection("/user/reaper") ! "done"
      } else
        next.tell(e - 1, null);
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
  var n_rings:Int = 0
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
      case _ => next ! n_messages
    }
  }

  def main(args: Array[String]) {
    if (args.size < 3) {
      println("program <#actors> <#rings> <#msg>")
      System.exit(-1)
    }
    n_rings = args(0).toInt
    n_actors = args(1).toInt
    n_messages = args(2).toInt

    system.actorOf(Props(classOf[Reaper], n_rings), "reaper")
    for (i <- 1 to n_rings) {
      setup_ring(i)
    }
  }
}


// /* The Computer Language Benchmarks Game
//    http://benchmarksgame.alioth.debian.org/

//    contributed by Stefan Ettrup

// */
// import akka.actor.Actor
// import akka.actor.ActorRef
// import akka.actor.ActorSystem
// import akka.actor.Props
// import com.typesafe.config.Config
// import com.typesafe.config.ConfigFactory
// import com.typesafe.config.ConfigValueFactory
// import com.typesafe.config.impl.ConfigLong
// import java.io.File

// class ThreadRingActor(id : Int, master: ActorRef) extends Actor {
//   var next: ActorRef = null

//   def receive: Actor.Receive = {
//     case e: Integer => {
//       if (e == 0) {
//         println(id); 
//         master ! "done"
//         // System.exit(0)
//       } else
//         next.tell(e - 1, null);
//     }
//     case n: ActorRef => { this.next = n }
//   }
// }

// class Master(total: Int) extends Actor {
//   var dones = 0

//   def receive: Actor.Receive = {
//     case "done" => {
//       dones = dones + 1
//       if (dones == total) {
//         System.exit(0)
//       }
//     }
//   }
// }

// object Main {
//   // val ring = 503
//   var n_rings = 0
//   var n_actors = 0 
//   var n_messages = 0

//   def main(args: Array[String]) {
//     n_rings = args(0).toInt
//     n_actors = args(1).toInt
//     n_messages = args(2).toInt

//     val system = ActorSystem("system",config)
//     val master = system.actorOf(Props(classOf[Master], n_rings), "master")

//     for (j <- 1 to n_rings) {
//       var actors = (for (i <- 1 to n_actors)
//         yield system.actorOf(Props(classOf[ThreadRingActor],i,master), 
//                                (i).toString() + "ring" + (j).toString)).toArray

//       for (i <- 0 until n_actors) {
//         if (i == n_actors - 1) actors(i) ! actors(0)
//         else actors(i) ! actors(i + 1)
//       }

//       actors(0) ! n_messages.toInt
//     }
//   }
  
//   //Akka configuration change
//   // val config = ConfigFactory.parseString("""
//   // akka{actor{default-dispatcher{fork-join-executor{parallelism-max = 1}}}}""").
//   // withFallback(ConfigFactory.defaultReference(this.getClass.getClassLoader))
// }
