package tetris

import java.awt.{Graphics2D, Color}
import java.awt.geom._
import java.awt.image.BufferedImage
import swing.Component


class Shape(var layout: Array[Array[Int]], val image: BufferedImage) {
  
  var x = 4
  
  var y = 0
  
  
  def rotate() = {
    var newLayout = this.layout.transpose.map(_.reverse)
    var canRotate = true
    newLayout.indices.foreach(y => newLayout(y).indices.foreach(x => 
    if(newLayout(y)(x) == 1) {
      if(this.x + x >= 11) this.x -= this.x + x - 11
      else if(this.x + x <= 2) this.x += 2 - this.x + x  
    }))
    this.layout = newLayout
  }
  
  
  def show(g: Graphics2D) = {
    layout.indices.foreach(row => layout(row).indices.foreach(square => 
    if(layout(row)(square) == 1) g.drawImage(image, 32 * (x + square), 32 * (y + row), null)))
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