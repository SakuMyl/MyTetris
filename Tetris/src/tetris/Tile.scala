package tetris
import swing._
import java.awt.image.BufferedImage
import java.awt.Graphics2D


class Tile(val image: BufferedImage, var x: Int, var y: Int) {
 
  def show(g: Graphics2D) = {
    g.drawImage(image, 32 * x, 32 * y, null)
  }
  val size = 32
  
}