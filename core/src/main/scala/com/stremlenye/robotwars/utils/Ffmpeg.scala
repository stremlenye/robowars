package com.stremlenye.robotwars.utils

import java.nio.file.Path

object Ffmpeg {
  def composeCommand(frameRate : Int, sourceImagesPath : Path, output : Path) : String =
    s"ffmpeg -y -framerate ${frameRate} -i ${sourceImagesPath.toString}/%05d.png $output"
}
