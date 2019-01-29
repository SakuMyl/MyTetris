package tetris

import java.awt.Graphics2D
import java.awt.image.BufferedImage
import swing._
import tetris._


class Shape(var layout: Array[Array[Int]], val image: BufferedImage) {
  
  //The block's coordinates on the game grid
  var x = 5
  
  var y = 0
  
  //Index of the tile most on the left in the block's layout. 
  def leftSide = {
    val transposed = layout.transpose
    transposed.indices.filter(column => transposed(column).exists(_==1)).min
  }
  
  //Index of the tile most on the right in the block's layout.
  def rightSide = {
    val transposed = layout.transpose
    transposed.indices.filter(column => transposed(column).exists(_==1)).max
  }
  
  //Determines the center of the block. Used for placing the block in the center of the next block -field.  
  val centerPoint = {
    val transposed = layout.transpose
    val rowsWithTiles = {
      layout.indices.filter(rowIndex => layout(rowIndex).exists(_ == 1))
    }
    ((rightSide + leftSide) / 2.0, rowsWithTiles.sum.toDouble / rowsWithTiles.size)
    
  }
  
  //Rotates the block counter-clockwise
  def rotate() = {
    var newLayout = this.layout.transpose.map(_.reverse)
    if(TetrisApp.locationAllowed(newLayout, this.x, this.y)) {
      newLayout.indices.foreach{y => newLayout(y).indices.foreach{x => 
        if(newLayout(y)(x) == 1) {
          if(this.x + x >= 10)  this.x -= this.x + x - 10
          else if(this.x + x <= 1) this.x += 1 - this.x + x  
        }
      }}
      this.layout = newLayout
    }
  }
  
  //Draws all tiles of this block in the graphic component
  def show(g: Graphics2D, xPos:Int = this.x, yPos:Int = this.y) = {
    layout.indices.foreach(row => layout(row).indices.foreach(square => 
    if(layout(row)(square) == 1) g.drawImage(image, 32 * (x + square), 32 * (y + row), null)))
  }
  //Moves the block left, if possible
  def moveLeft() = {
    if(canMoveLeft)this.x -= 1
  }
  //Determines whether the block can be moved left
  def canMoveLeft = {
    TetrisApp.locationAllowed(this.layout, this.x - 1, this.y) && !TetrisApp.isOutOfBounds(this.layout, this.x - 1, this.y)
//    layout.indices.forall(y => layout(y).indices.forall(x => 
//    (this.x + x > 1 && !TetrisApp.lockedImages.exists(tile => tile.x == this.x + x - 1 && tile.y == this.y + y)) || layout(y)(x) == 0))
  }
  //Moves the block right, if possible
  def moveRight() = {
    if(canMoveRight) this.x += 1
  }
  //Determines whether the block can be moved right
  def canMoveRight = {
    TetrisApp.locationAllowed(this.layout, this.x + 1, this.y) && !TetrisApp.isOutOfBounds(this.layout, this.x + 1, this.y)
//    layout.indices.forall(y => layout(y).indices.forall(x => 
//    (this.x + x < 10 && !TetrisApp.lockedImages.exists(tile => tile.x == this.x + x + 1 && tile.y == this.y + y)) || this.layout(y)(x) == 0))
  }
  //Determines whether the block touches the bottom or other tiles below the block
  def isLocked = {
    !TetrisApp.locationAllowed(this.layout, this.x, this.y + 1) || TetrisApp.isOutOfBounds(this.layout, this.x, this.y + 1)
//    layout.indices.exists(y => layout(y).indices.exists(x => 
//    (this.layout(y)(x) == 1 && TetrisApp.lockedImages.exists(tile => tile.x == this.x + x && tile.y == this.y + y + 1)) || isOnBoundary))
  }
  
  //Determines whether the block touches the bottom
//  def isOnBoundary = {
//    this.layout.indices.exists(y => this.layout(y).indices.exists(x => 
//      this.layout(y)(x) == 1 && this.y + y >= TetrisApp.GridHeight - 1))
//  }
  
  //Moves the block downwards 
  def fall() = this.y += 1
  
}