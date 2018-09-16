package com.stremlenye.robotwars.utils.algebras

import cats._
import cats.tagless._
import com.typesafe.scalalogging.Logger

@finalAlg
@autoFunctorK
trait LoggingAlgebra[F[_]] {
  def info(msg : String) : F[Unit]
  def warn(msg : String) : F[Unit]
  def error(msg : String) : F[Unit]
  def error(msg : String, cause : Throwable) : F[Unit]
  def trace(msg : String) : F[Unit]
}

final class SimpleLogging[F[_] : Applicative](logger : Logger) extends LoggingAlgebra[F] {
  def info(msg : String) : F[Unit] = Applicative[F].pure(logger.info(msg))

  def warn(msg : String) : F[Unit] = Applicative[F].pure(logger.warn(msg))

  def error(msg : String) : F[Unit] = Applicative[F].pure(logger.error(msg))

  def error(msg : String, cause : Throwable) : F[Unit] = Applicative[F].pure(logger.error(msg, cause))

  def trace(msg : String) : F[Unit] = Applicative[F].pure(logger.trace(msg))
}

object SimpleLogging {
  def apply[F[_] : Applicative](loggerName : String) : SimpleLogging[F] =
    new SimpleLogging(Logger(loggerName))
}


