package week2

trait Subscriber {
  def handler(publ: Publisher)
}