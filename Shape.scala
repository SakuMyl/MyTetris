package tetris

import tetris._
import java.awt.{Graphics2D, Color}
import java.awt.geom._
import java.awt.image.BufferedImage
import swing.Component


class Shape(var layout: Array[Array[Int]], image: BufferedImage) {
  
  var x = 4
  
  var y = 0
  
  
  def rotate() = {
    this.layout = layout.transpose
  }
  
  
  def show(g: Graphics2D) = {
    for(row <- layout.indices) {
      for(square <- layout(row).indices) {
        if(layout(row)(square) == 1) {
          g.drawImage(image, 32 * (x + square), 32 * (y + row), null)
        }
      }
    }
  }
  
  def moveLeft() = this.x -= 1 
  
  def moveRight() = {
    this.x += 1

  }
  
  def fall() = this.y += 1
  
}