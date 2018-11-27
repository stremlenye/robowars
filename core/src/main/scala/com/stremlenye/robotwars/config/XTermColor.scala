package com.stremlenye.robotwars.config

import com.sksamuel.scrimage.RGBColor
import io.circe.Decoder
import io.circe.generic.semiauto._

case class XTermColor(colorId : Int, rgb : RGBColor)

object XTermColor {
  implicit val rgbColorDecoder : Decoder[RGBColor] = Decoder.forProduct3("r", "g", "b")(RGBColor(_ : Int, _ : Int, _ : Int, 255))

  implicit val decoder : Decoder[XTermColor] = deriveDecoder
}
