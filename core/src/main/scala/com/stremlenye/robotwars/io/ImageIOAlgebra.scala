package com.stremlenye.robotwars.io

import cats.tagless._
import com.sksamuel.scrimage._
import com.stremlenye.robotwars.mtl.FunctorKDerivation

trait ImageIOAlgebra[F[_]] {
  def sink(frameNumber : Long, image : Image) : F[Unit]
}

object ImageIOAlgebra {
  implicit def functorK : FunctorK[ImageIOAlgebra] = FunctorKDerivation.apply[ImageIOAlgebra]
}
