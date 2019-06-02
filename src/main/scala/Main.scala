import monad._

object Main extends App {

  implied for Monad[List] {
    def pure[A]: A => List[A] = List(_)
    def flatMap[A, B](as: List[A])(f: A => List[B]): List[B] = as flatMap f
  }

  def split(d: String): Kleisli[List, String, String] = 
    Kleisli(input => input.split(d).toList)

  def multiplyBy(k: Int): Kleisli[List, String, String] = 
    Kleisli(str => List.tabulate(k)(_ => str))

  import functionK._
  val headOption: List ~> Option = [A] => (l: List[A]) => l.headOption

  println( List(1, 2, 3) `<$>` (_ + 10) >>= (x => List(x * 2)) )
  println( (split(";") >>> multiplyBy(2)) <| "x;y;z" )
  println( multiplyBy(3) ||> headOption <| "42" )
}
