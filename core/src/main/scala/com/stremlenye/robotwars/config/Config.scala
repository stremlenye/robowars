package com.stremlenye.robotwars.config

import com.stremlenye.robotwars.utils.files

object Config {
  lazy val xtermColors : Vector[XTermColor] =
    files.readResource[Vector[XTermColor]]("/xterm_colors.json").fold(e => sys.error(e.getLocalizedMessage), identity)
}
