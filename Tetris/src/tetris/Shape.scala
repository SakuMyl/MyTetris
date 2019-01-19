package tetris

import java.awt.{Graphics2D, Color}
import java.awt.geom._
import java.awt.image.BufferedImage
import swing.Component


class Shape(var layout: Array[Array[Int]], val image: BufferedImage) {
  
  var x = 4
  
  var y = 0
  
  
  def rotate() = {
    this.layout = layout.transpose
    var newLayout = this.layout
//    layout.indices.foreach(row => newLayout(row) = this.layout(layout.size - row - 1))
    this.layout = newLayout
  }
  
  
  def show(g: Graphics2D) = {
    layout.indices.foreach(row => layout(row).indices.foreach(square => 
    if(layout(row)(square) == 1) g.drawImage(image, 32 * (x + square), 32 * (y + row), null)))
    println(x)
  }
  
  def moveLeft() = {
    var canMove = true
    this.layout.indices.foreach(y => this.layout(y).indices.foreach(x => 
    if(this.x + x <= 2 && this.layout(y)(x) == 1) canMove = false))
    if(canMove) this.x -= 1

  }
  
  def moveRight() = {
    var canMove = true
    this.layout.indices.foreach(y => this.layout(y).indices.foreach(x => 
    if(this.x + x >= 11 && this.layout(y)(x) == 1) canMove = false))
    if(canMove) this.x += 1
  }
  
  def fall() = this.y += 1
  
}