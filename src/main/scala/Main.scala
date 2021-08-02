import cats._
import cats.implicits._
import reactives.Signal
import reactives.Event
import reactives.Event.RootEvent

object Main {

  def main(args: Array[String]): Unit = {

    val s = (Event[Int] <+> Event[Int].filter(_ % 2 == 0)).fold(0)(_ + _)
    
    val (a, ctx0) = s.init
    println(a)
    val (b, ctx1) = s.update(10, ctx0)
    println(b)
    val (c, ctx2) = s.update(11, ctx1)
    println(c)
    val (d, ctx3) = s.update(12, ctx2)
    println(d)
  }
}