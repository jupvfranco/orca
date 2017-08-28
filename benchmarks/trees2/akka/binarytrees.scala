import akka.actor.Actor
import akka.actor.ActorRef
import akka.actor.ActorSystem
import akka.actor.Props
import com.typesafe.config.Config
import com.typesafe.config.ConfigFactory
import scala.collection.mutable.HashMap

object const {
  val minDepth = 4
}

case class Reply(a:ActorRef, ret:(Int,Int,Int))

class Worker(minDepth: Int, maxDepth: Int) extends Actor {
  def run(depth: Int) = {
    val n = 1 << (maxDepth - depth + minDepth)
    var sum = 0
    for (i <- n until 0 by -1)
      sum += Tree(i, depth).isum + Tree(-i, depth).isum
    context.actorSelection("/user/master") ! Reply(self, (2*n, depth, sum))
  }

  def receive = {
    case d:Int => run(d)
    case _ => { println("sth") }
  }
}

class Master(minDepth: Int, maxDepth: Int, depths: List[Int]) extends Actor {
  var longLivedTree:Tree = null
  var children:Int = depths.length
  val results:HashMap[ActorRef, (Int, Int, Int)] = HashMap.empty
  var actors:List[ActorRef] = null

  def report(name: String, depth: Int, check: Int) =
    println(name + " of depth " + depth + "\t check: " + check)

  override
  def preStart = {
    report("stretch tree", maxDepth+1, Tree(0,maxDepth+1).isum)
    longLivedTree = Tree(0,maxDepth)
    actors = (
        for ((d,i) <- depths.zipWithIndex)
          yield {
            val a = context.actorOf(
              Props(classOf[Worker], minDepth, maxDepth), (i).toString);
            a ! d
            a
          }
        ).toList
  }

  def finished = {
    for (a <- actors) {
      val ret = results.get(a).get
      report(ret._1 + "\t trees", ret._2, ret._3)
    }
    report("long lived tree", maxDepth, longLivedTree.isum)
  }

  def receive = {
    case Reply(a, ret) => {
      results += (a -> ret)
      if (this.children == 1) {
        context.system.shutdown()
        finished
      } else {
        this.children -= 1
      }
    }
    case _ => { println("sth") }
  }
}

final class Tree(i: Int, left: Tree, right: Tree) {
  def isum: Int = if (left eq null) i else i + left.isum - right.isum
}
object Tree {
  def apply(i: Int, depth: Int): Tree = {
    if (depth > 0) new Tree(i, Tree(i*2-1, depth-1), Tree(i*2, depth-1))
    else new Tree(i, null, null)
  }
}

object Main {
  val config = ConfigFactory.load(ConfigFactory.parseString("""
          akka {
            log-dead-letters-during-shutdown = off
          }
        """))

  val system = ActorSystem("system", config)

  def main(args: Array[String]) {
    val n = try{ args(0).toInt } catch { case _ : Throwable => 1 }
    val minDepth = const.minDepth
    val maxDepth = n max (minDepth+2)

    val range = minDepth to maxDepth by 2
    // val depths = range.toList
    val count = 128
    val depths = List.fill(Math.ceil(count.toDouble/range.length).toInt)(range)
                  .flatten
                  .take(count)
    system.actorOf(
        Props(classOf[Master], minDepth, maxDepth, depths),"master")
  }
}
