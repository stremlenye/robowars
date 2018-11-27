package com.stremlenye.robotwars.utils

import com.sksamuel.scrimage.{Image, Pixel}

object images {
  def iterable(image: Image) : Iterator[(Int, Int, Pixel)] = {
    var aggr = Vector.empty[(Int, Int, Pixel)]
    image.foreach((x, y, p) => aggr = aggr :+ ((x,y,p)))
    aggr.iterator
  }
}
