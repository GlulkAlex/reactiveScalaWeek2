package week2

/*to maintain set of subScribers*/
trait Publisher {
  private var subscribers: Set[ Subscriber ] = Set()

  def subscribe( subscriber: Subscriber ): Unit =
    subscribers += subscriber

  def unsubscribe( subscriber: Subscriber ): Unit =
    subscribers -= subscriber

  def publish(): Unit =
    /*'.handler' method provided by 'Subscriber'*/
    subscribers
      .foreach { x => x.handler( this ) /*or just '_.handler(this)'*/ }
}