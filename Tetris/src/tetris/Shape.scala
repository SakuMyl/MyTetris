package tetris

import java.awt.Graphics2D
import java.awt.image.BufferedImage


class Shape(var layout: Array[Array[Int]], val image: BufferedImage) {
  
  var x = 5
  
  var y = 0
  
  val centerPoint = {
    val transposed = layout.transpose
    val rowsWithTiles = {
      layout.indices.filter(rowIndex => layout(rowIndex).exists(_ == 1))
    }
    val columnsWithTiles = {
      transposed.indices.filter(rowIndex => transposed(rowIndex).exists(_ == 1))
    }
    (columnsWithTiles.sum.toDouble / columnsWithTiles.size, rowsWithTiles.sum.toDouble / rowsWithTiles.size)
  }
  println(centerPoint)
  
//  var tileLayout = Array.ofDim[Tile](layout.size, layout.size)
  
//  layout.indices.foreach(y => layout(y).indices.foreach(x => tileLayout(y)(x) = new Tile(image, this.x + x, this.y + y)))
  
  def rotate() = {
    var newLayout = this.layout.transpose.map(_.reverse)
    newLayout.indices.foreach(y => newLayout(y).indices.foreach(x => 
    if(newLayout(y)(x) == 1) {
      if(this.x + x >= 10) this.x -= this.x + x - 10
      else if(this.x + x <= 1) this.x += 1 - this.x + x  
    }))
    this.layout = newLayout
  }
  
  
  def show(g: Graphics2D, xPos:Int = this.x, yPos:Int = this.y) = {
    layout.indices.foreach(row => layout(row).indices.foreach(square => 
    if(layout(row)(square) == 1) g.drawImage(image, 32 * (x + square), 32 * (y + row), null)))
  }
  
  def moveLeft() = {
    this.x -= 1
  }
  
  def moveRight() = {
    this.x += 1
  }
  
  def fall() = this.y += 1
  
}