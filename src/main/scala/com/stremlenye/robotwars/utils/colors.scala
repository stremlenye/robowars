package com.stremlenye.robotwars.utils

import com.sksamuel.scrimage.Color

import scala.util.Random


object colors {
  def randomColor(seed : Int) : Color = {
    val rnd = new Random(seed)
    Color(
      red = rnd.nextInt(255),
      green = rnd.nextInt(255),
      blue = rnd.nextInt(255),
    )
  }
}
