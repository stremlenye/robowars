package com.stremlenye.robotwars.io

import java.nio.file.Path

import cats.MonadError
import cats.syntax.all._
import com.sksamuel.scrimage.Image
import com.stremlenye.robotwars.utils.algebras.LoggingAlgebra

class FrameFilesOutput[F[_]](basePath : Path,
                             logger : LoggingAlgebra[F])(implicit F : MonadError[F, Throwable]) extends ImageIOAlgebra[F] {

  def sink(frameNumber : Long, image : Image) : F[Unit] = {
    val fileName = "%05d.png".format(frameNumber)

    for {
      _ <- logger.info(s"Saving image to $fileName")
      out <- F.catchNonFatal {
        image.output(basePath.resolve(s"./$fileName"))
        ()
      }
      _ <- logger.info(s"Saved image")
    } yield out
  }
}
