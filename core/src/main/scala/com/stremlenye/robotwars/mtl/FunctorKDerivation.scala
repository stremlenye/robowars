package com.stremlenye.robotwars.mtl

import cats.tagless.FunctorK
import com.stremlenye.robotwars.macros.FunctorKMacros

object FunctorKDerivation {
  def apply[Alg[_[_]]] : FunctorK[Alg] = macro FunctorKMacros.derive[Alg]
}
