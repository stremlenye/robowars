package com.stremlenye.robotwars.mtl

import cats._
import cats.implicits._
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra

object Transformations {
  def logging[F[_] : Monad](logger : LoggingAlgebra[F])(
    before : String,
    after : String
  ) : F ~> F =
    new (F ~> F) {
      override def apply[A](fa : F[A]) : F[A] =
        for {
          _ <- logger.trace(before)
          a <- fa
          _ <- logger.trace(after)
        } yield a
    }

  def errorLogging[F[_] : MonadError[?[_], Throwable]](logger : LoggingAlgebra[F]) : F ~> F =
    new (F ~> F) {
      override def apply[A](fa : F[A]) : F[A] =
        fa.handleErrorWith { e =>
          logger.error(e.getMessage, e)
            .flatMap(_ => fa)
        }
    }

  def trilogging[F[_] : MonadError[?[_], Throwable]](logger : LoggingAlgebra[F])(
    before : String,
    after : String
  ) : F ~> F =
    logging(logger)(before, after) andThen errorLogging(logger)
}
