package monad

import scala.annotation.{alpha, infix}

trait Monad[F[_]] {
  def pure[A]: A => F[A]
  @infix @alpha(">>=") def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  @infix @alpha("<$>") def map[A, B](fa: F[A])(f: A => B): F[B] = flatMap(fa)(pure compose f)
}