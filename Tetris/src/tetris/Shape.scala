package tetris

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import swing._
import tetris._


class Shape(var layout: Array[Array[Int]], val image: BufferedImage) {
  
  var x = 5
  
  var y = 0
  
  def leftSide = {
    val transposed = layout.transpose
    transposed.indices.filter(column => transposed(column).exists(_==1)).min
  }
  
  def rightSide = {
    val transposed = layout.transpose
    transposed.indices.filter(column => transposed(column).exists(_==1)).max
  }
  
  val centerPoint = {
    val transposed = layout.transpose
    val rowsWithTiles = {
      layout.indices.filter(rowIndex => layout(rowIndex).exists(_ == 1))
    }
    ((rightSide + leftSide) / 2.0, rowsWithTiles.sum.toDouble / rowsWithTiles.size)
    
  }
  
  
  def rotate() = {
    var newLayout = this.layout.transpose.map(_.reverse)
    newLayout.indices.foreach{y => newLayout(y).indices.foreach{x => 
      if(newLayout(y)(x) == 1) {
        if(this.x + x >= 10) this.x -= this.x + x - 10
        else if(this.x + x <= 1) this.x += 1 - this.x + x  
      }
    }}
    this.layout = newLayout
  }
  
  
  def show(g: Graphics2D, xPos:Int = this.x, yPos:Int = this.y) = {
    layout.indices.foreach(row => layout(row).indices.foreach(square => 
    if(layout(row)(square) == 1) g.drawImage(image, 32 * (x + square), 32 * (y + row), null)))
  }
  
  def moveLeft() = {
    if(canMoveLeft)this.x -= 1
  }
  //Determines whether the block can be moved left
  def canMoveLeft = {
    layout.indices.forall(y => layout(y).indices.forall(x => 
    (this.x + x > 1 && !TetrisApp.lockedImages.exists(tile => tile.x == this.x + x - 1 && tile.y == this.y + y)) || layout(y)(x) == 0))
  }
  
  def moveRight() = {
    if(canMoveRight) this.x += 1
  }
  //Determines whether the block can be moved right
  def canMoveRight = {
    layout.indices.forall(y => layout(y).indices.forall(x => 
    (this.x + x < 10 && !TetrisApp.lockedImages.exists(tile => tile.x == this.x + x + 1 && tile.y == this.y + y)) || this.layout(y)(x) == 0))
  }
  //Determines whether there is the bottom or other tiles below the block
  def isLocked = {
    layout.indices.exists(y => layout(y).indices.exists(x => 
    (this.layout(y)(x) == 1 && TetrisApp.lockedImages.exists(tile => tile.x == this.x + x && tile.y == this.y + y + 1)) || isOnBoundary))
  }
  
  //Determines whether the block touches the bottom
  def isOnBoundary = {
    this.layout.indices.exists(y => this.layout(y).indices.exists(x => 
      this.layout(y)(x) == 1 && this.y + y >= TetrisApp.GridHeight - 1))
  }
  
  def fall() = this.y += 1
  
}