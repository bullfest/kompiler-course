package punkt0

import java.io.File

object Positioned {
  val LINE_BITS   = 20
  val COLUMN_BITS = 31 - LINE_BITS
  val LINE_MASK   = (1 << LINE_BITS) - 1
  val COLUMN_MASK = (1 << COLUMN_BITS) - 1

  def lineOf(pos: Int): Int =
    (pos >> COLUMN_BITS) & LINE_MASK

  def columnOf(pos: Int): Int =
    pos & COLUMN_MASK
}

trait Positioned {
  private var info: Option[(File, Int)] = None

  def file   = info.get._1
  def line   = Positioned.lineOf(info.get._2)
  def column = Positioned.columnOf(info.get._2)

  def hasPos = info.isDefined

  def setPos(file: File, pos: Int): Unit = {
    info = Some((file, pos))
  }

  def setPos(that: Positioned): Unit = {
    info = that.info
  }

  def posString: String =
    if (hasPos) file.getPath + ":" + line + ":" + column
    else "?:?"
}
