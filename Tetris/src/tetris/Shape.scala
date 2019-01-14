package tetris

import java.awt.{Graphics2D, Color}
import java.awt.geom._
import java.awt.image.BufferedImage
import swing.Component


class Shape(val layout: Array[Array[Int]], image: BufferedImage) {
  
  var x = 4
  
  var y = 0
  
  
  def rotate = ???
  
  def show(g: Graphics2D) = {
    for(row <- layout.indices) {
      for(square <- layout(row).indices) {
        if(square == 1) {
          g.drawImage(image, 32 * (x + row), 32 * (y + square), null)
        }
      }
    }
  }
  
  def moveLeft() = this.x -= 1 
  
  def moveRight() = this.x += 1
  
  def fall() = this.y += 1
  
}