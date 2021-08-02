package reactives

import reactives.Event.RootEvent
import reactives.Reactive.Context

trait Reactive[+A] {
  
  def init: (A, Context) = eval(Context(Map(), Map()))
  
  def update(action: Any, ctx: Context): (A, Context) =
    eval(ctx.reset.updateEvent(RootEvent, Seq(action)))
  
  private[reactives] def eval(ctx: Context): (A, Context)
}

object Reactive {
  
  case class Context (
    signals: Map[Signal[_], SignalContext[_]],
    events: Map[Event[_], EventContext[_]]
  ) {
    
    def updateSignal[T](signal: Signal[T], value: T): Context = copy (
      signals = signals + (signal -> SignalContext (
        signal = signal,
        value = value,
        modified = signals.get(signal).map(_.value).contains(value)
      ))
    )
    
    def updateEvent[T](event: Event[T], value: Seq[T]): Context = copy (
      events = events + (event -> EventContext (
        event = event,
        value = value,
        modified = events.get(event).map(_.value).contains(value)
      ))
    )
    
    def savedSignal[T](signal: Signal[T], parents: Set[Reactive[_]]): Option[T] =
      signals.get(signal).map(_.value.asInstanceOf[T])
        .filter(_ => allUnmodified(parents))
    
    def savedEvent[T](event: Event[T], parents: Set[Reactive[_]]): Option[Seq[T]] =
      events.get(event).map(_.value.map(_.asInstanceOf[T]))
        .filter(_ => allUnmodified(parents))
        
    def reset: Context = copy (
      signals = signals.view.mapValues(_.copy(modified = false)).toMap,
      events = events.view.mapValues(_.copy(modified = false)).toMap
    )
        
    private def allUnmodified(reactives: Set[Reactive[_]]): Boolean = {
      reactives.forall {
        case s: Signal[_] => !signals.get(s).exists(_.modified)
        case e: Event[_] => !events.get(e).exists(_.modified)
      }
    }
  }
  
  case class SignalContext[T] (
    signal: Signal[T],
    value: T,
    modified: Boolean
  )
  
  case class EventContext[T] (
    event: Event[T],
    value: Seq[T],
    modified: Boolean
  )
}