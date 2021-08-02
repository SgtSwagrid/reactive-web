package reactives

import cats._
import cats.implicits._
import reactives.Reactive._
import reactives.Event._
import reactives.Signal._

sealed trait Event[+A] extends Reactive[Seq[A]] {
  
  def filter(f: A => Boolean): Event[A] = FilteredEvent(this, f)
  
  def hold: Signal[Option[A]] = HoldSignal(this)
  
  def fold[B](x: B)(f: (B, A) => B): Signal[B] = FoldSignal(this, x, f)
}

object Event {
  
  case class MappedEvent[A, B](fa: Event[A], f: A => B) extends Event[B] {
    
    def eval(ctx0: Context): (Seq[B], Context) = {
      val (a, ctx1) = fa.eval(ctx0)
      val b = a.map(f) //ctx1.savedEvent(this, Set(fa)).getOrElse(a.map(f))
      (b, ctx1.updateEvent(this, b))
    }
  }
  
  case class CombinedEvent[A](x: Event[A], y: Event[A]) extends Event[A] {
    
    def eval(ctx0: Context): (Seq[A], Context) = {
      val (a, ctx1) = x.eval(ctx0)
      val (b, ctx2) = y.eval(ctx1)
      (a ++ b, ctx2.updateEvent(this, a ++ b))
    }
  }
  
  case class FilteredEvent[A](x: Event[A], f: A => Boolean) extends Event[A] {
    
    def eval(ctx0: Context): (Seq[A], Context) = {
      val (a, ctx1) = x.eval(ctx0)
      val b = a.filter(f) //ctx1.savedEvent(this, Set(x)).getOrElse(a.filter(f))
      (b, ctx1.updateEvent(this, b))
    }
  }
  
  case class NullEvent[A]() extends Event[A] {
    
    def eval(ctx: Context): (Seq[A], Context) = {
      (Seq(), ctx.updateEvent(this, Seq()))
    }
  }
  
  case object RootEvent extends Event[Any] {
    
    def eval(ctx: Context): (Seq[Any], Context) = {
      (ctx.savedEvent(this, Set()).getOrElse(Seq()), ctx)
    }
  }
  
  def apply[A]: Event[A] =
    RootEvent.filter(_.isInstanceOf[A]).map(_.asInstanceOf[A])
  
  implicit def eventFunctor: Functor[Event] = new Functor[Event] {
    override def map[A, B](fa: Event[A])(f: A => B): Event[B] = MappedEvent(fa, f)
  }
  
  implicit def eventMonoid: MonoidK[Event] = new MonoidK[Event] {
    override def empty[A]: Event[A] = NullEvent()
    override def combineK[A](x: Event[A], y: Event[A]): Event[A] = CombinedEvent(x, y)
  }
}