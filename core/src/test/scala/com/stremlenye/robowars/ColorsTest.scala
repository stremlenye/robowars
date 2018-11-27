package com.stremlenye.robowars

import com.sksamuel.scrimage.{Color, RGBColor}
import com.stremlenye.robotwars.config.{Config, XTermColor}
import org.scalatest.{FunSuite, Matchers}

class ColorsTest extends FunSuite with Matchers {
  private def cartesianDistance(l : RGBColor, r : RGBColor) : Int =
    math.abs(l.red - r.red) +
      math.abs(l.green - r.green) +
      math.abs(l.blue - r.blue)

  private def findClosestColorID(color : RGBColor) : Int =
    Config.xtermColors.minBy {
      case XTermColor(_, rgb) => cartesianDistance(rgb, color)
    }.colorId

  test("Cartesian color search") {
    assert(findClosestColorID(Color.awt2color(java.awt.Color.RED)) == 31)
    val color = Color.awt2color(java.awt.Color.BLUE).toRGB
  }

}
