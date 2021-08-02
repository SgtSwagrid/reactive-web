package reactives

import cats.Monad
import reactives.Reactive._

sealed trait Signal[+A] extends Reactive[A]

object Signal {

  case class PureSignal[A](x: A) extends Signal[A] {
    
    override def eval (ctx: Context): (A, Context) = {
      (x, ctx.updateSignal(this, x))
    }
  }
  
  case class MonadicSignal[A, B](fa: Signal[A], f: A => Signal[B]) extends Signal[B] {
    
    def eval(ctx0: Context): (B, Context) = {
      
      val (a, ctx1) = fa.eval(ctx0)
      val fb = f(a)
      
      val (b, ctx2) = ctx1.savedSignal(this, Set(fa, fb)) match {
        case Some(b) => (b, ctx1)
        case None => fb.eval(ctx1)
      }
  
      (b, ctx2.updateSignal(this, b))
    }
  }
  
  case class HoldSignal[A](x: Event[A]) extends Signal[Option[A]] {
    
    def eval(ctx0: Context): (Option[A], Context) = {
      val (a, ctx1) = x.eval(ctx0)
      (a.lastOption, ctx1.updateSignal(this, a.lastOption))
    }
  }
  
  case class FoldSignal[A, B](x: Event[A], a: B, f: (B, A) => B) extends Signal[B] {
  
    def eval(ctx0: Context): (B, Context) = {
      val (b, ctx1) = x.eval(ctx0)
      val current = ctx1.signals.get(this).map(_.value.asInstanceOf[B])
      val c = b.foldLeft(current.getOrElse(a))(f)
      (c, ctx1.updateSignal(this, c))
    }
  }
  
  def apply[A](x: A): Signal[A] = PureSignal(x)

  implicit def signalMonad: Monad[Signal] = new Monad[Signal] {
    
    override def pure[A](x: A): Signal[A] = PureSignal(x)
    override def flatMap[A, B](fa: Signal[A])(f: A => Signal[B]): Signal[B] =
      MonadicSignal(fa, f)
    
    override def tailRecM[A, B](a: A)(f: A => Signal[Either[A, B]]): Signal[B] = {
      flatMap(f(a)) {
        case Right(b) => pure(b)
        case Left(nextA) => tailRecM(nextA)(f)
      }
    }
  }
}