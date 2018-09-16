package com.stremlenye.robotwars.io

import cats.implicits._
import java.nio.file.Path
import com.sksamuel.scrimage._
import com.sksamuel.scrimage.nio.ImageWriter._

object ImageIO {
  def out(frameNumber : Long, image: Image)(path : Path) : Either[Throwable, Unit] = {
    Either.catchNonFatal {
      image.output(path.resolve(s"./$frameNumber.png"))
      ()
    }
  }
}
