package com.stremlenye.robotwars.io

import java.nio.file.Path

import cats._
import cats.implicits._
import cats.tagless._
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.nio.ImageWriter._
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra

@finalAlg
@autoFunctorK
trait ImageIOAlgebra[F[_]] {
  def sink(frameNumber : Long, image : Image) : F[Unit]
}

object ImageIOAlgebra {
  def apply[F[_] : MonadError[?[_], Throwable]](basePath : Path,
                                                logger : LoggingAlgebra[F]) : ImageIOAlgebra[F] =
    new ImageIOAlgebra[F] {
      override def sink(frameNumber : Long, image : Image) : F[Unit] = {
        val fileName = "%05d.png".format(frameNumber)

        for {
          _ <- logger.info(s"Saving image to $fileName")
          out <- MonadError[F, Throwable].catchNonFatal {
            image.output(basePath.resolve(s"./$fileName"))
            ()
          }
          _ <- logger.info(s"Saved image")
        } yield out
      }
    }
}
