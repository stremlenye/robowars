package com.stremlenye.robotwars.utils.algebras

import cats._
import cats.implicits._
import cats.tagless.{autoFunctorK, finalAlg}

import sys.process._

@finalAlg
@autoFunctorK
trait ExternalProcessAlgebra[F[_]] {
  def run : F[Unit]
}

object ExternalProcessAlgebra {

  private[algebras] def processLogger[F[_]](logger : LoggingAlgebra[F])(implicit F : MonadError[F, Throwable]) : ProcessLogger =
    new ProcessLogger {
      override def out(s : => String) : Unit = {
        logger.trace(s)
        ()
      }

      override def err(s : => String) : Unit = {
        logger.error(s)
        ()
      }

      override def buffer[T](f : => T) : T = f
    }

  def apply[F[_]](command : String, logger : LoggingAlgebra[F])(implicit F : MonadError[F, Throwable]) : ExternalProcessAlgebra[F] =
    new ExternalProcessAlgebra[F] {
      override def run : F[Unit] =
        for {
          _ <- logger.trace(s"Starting process with command: $command")
          _ <- F.pure(command ! processLogger(logger))
          _ <- logger.trace(s"Finished process with command: $command")
        } yield ()
    }
}
