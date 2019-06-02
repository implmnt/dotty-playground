package monad

import functionK._
import scala.annotation.{alpha, infix}

opaque type Kleisli[F[_], A, B] = A => F[B]

object Kleisli {
  def apply[F[_], A, B](f: A => F[B]): Kleisli[F, A, B] = f

  implied {
    def (k: Kleisli[F, A, B]) >>> [F[_], A, B, C] (k2: Kleisli[F, B, C]) given Monad[F]: Kleisli[F, A, C] = k(_) >>= k2
    def (k: Kleisli[F, A, B]) <| [F[_], A, B] : A => F[B] = k
    @infix @alpha("||>") def mapK[F[_], G[_], A, B](k: Kleisli[F, A, B])(fK: F ~> G): Kleisli[G, A, B] = (e: A) => fK(k(e))
  }

  implied [F[_], E] for Monad[[R] =>> Kleisli[F, E, R]] given Monad[F] {
    def pure[A]: A => Kleisli[F, E, A] = a => (_: E) => the[Monad[F]].pure(a)
    def flatMap[A, B](fa: Kleisli[F, E, A])(f: A => Kleisli[F, E, B]): Kleisli[F, E, B] =
      (e: E) => the[Monad[F]].flatMap(fa(e))(f(_)(e))
  }
}