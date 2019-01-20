package tetris

import swing._
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import swing.event._
import java.awt.event._
import java.util.{Timer, TimerTask}
import java.awt.{Graphics2D, Color}
import java.awt.geom._
import java.awt.RenderingHints
import scala.util.Random

object TetrisApp extends SimpleSwingApplication {
  
  
  val image = ImageIO.read(new File("tetris/src/spritesheet.png"))
  
  val TileSize = 32
  
  val GridHeight = 23
  
  val GridWidth = 10
  
  private var score = 0
  
  val cropped = image.getSubimage(0, 0, TileSize * 17, TileSize * 26)
  
  def emptyImage(w: Int, h: Int) = {
    new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  }
  
  val frame = this.emptyImage(cropped.getWidth(), cropped.getHeight()) // TODO
  val g = frame.createGraphics()
  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  
  
  val layoutMap: Map[String, Array[Array[Int]]] = {
    
     Map("j" -> Array(Array(0,1,0),
                      Array(0,1,0),
                      Array(1,1,0)),
                      
         "i" -> Array(Array(0,1,0,0),
                      Array(0,1,0,0),
                      Array(0,1,0,0),
                      Array(0,1,0,0)),
                      
         "s" -> Array(Array(1,0,0),
                      Array(1,1,0),
                      Array(0,1,0)),
                      
         "o" -> Array(Array(1,1),
                      Array(1,1)),
                      
         "l" -> Array(Array(0,1,0),
                      Array(0,1,0),
                      Array(0,1,1)),
                      
         "z" -> Array(Array(0,1,0),
                      Array(1,1,0),
                      Array(1,0,0)),
                      
         "t" -> Array(Array(0,0,0),
                      Array(1,1,1),
                      Array(0,1,0))
         
        )
              
  }
  
  val layouts = Vector("j", "i", "s", "o", "l", "z", "t")
 
  val grid = Array.ofDim[BufferedImage](image.getHeight(), image.getWidth())
  
  
  for(i <- 0 until image.getHeight() / TileSize) {
    for(j <- 0 until image.getWidth() / TileSize) {
      grid(i)(j) = image.getSubimage(j * TileSize, i * TileSize, TileSize, TileSize)
    }
  }
  
  var currentShape = newShape()

  var holdShape: Option[Shape] = None
  
  var points = 0
  
  

  def top = new MainFrame { 
    val pic = new Label { 
      icon = new ImageIcon(frame)
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += pic
      listenTo(keys, mouse.clicks)
      reactions += {
        case KeyPressed(_, Key.A, _, _) => 
            if(canMoveLeft) currentShape.moveLeft()
            currentShape.show(g)
        
        case KeyPressed(_, Key.D, _, _) => 
            if(canMoveRight) currentShape.moveRight()
            currentShape.show(g)
        case KeyPressed(_, Key.E, _, _) =>
            currentShape.rotate()
            currentShape.show(g)
        case KeyPressed(_, Key.S, _, _) =>
            fallInterval = 1
        case KeyReleased(_, Key.S, _, _) => 
            fallInterval = 20
        case KeyPressed(_, Key.Q, _, _) =>
            hold()
      
      }
      focusable = true
      requestFocus
    }
    
    val timer = new Timer()
    var fallInterval = 20
    var i = 1
    def startAnimating(interval: Int) = {
      val task = new TimerTask {
        def run() = {
          if(hasEnded) timer.cancel()
          i += 1
          if(isLocked) lock()
          if(i % fallInterval == 0) currentShape.fall()
          g.drawImage(cropped, TileSize, 0, null)
          drawLockedTiles()
          currentShape.show(g)
          pic.repaint()
        }
      }
      timer.schedule(task,0,interval)
    }
    
    startAnimating(30)
  }

 
  var time = 0
  
  def newShape() = {
    val r = Random.nextInt(7)
    new Shape(this.layoutMap(layouts(r)), grid(r)(0))
  }
  
  var lockedTiles = Array.ofDim[Int](image.getHeight(), image.getWidth())
  
  var lockedImages = Vector[Tile]()
  
  var lockedShapes = Vector[Shape]()
  
  def drawLockedTiles() = {
    lockedImages.foreach(tile => if(tile.isInstanceOf[Tile]) tile.show(g))
  }
  
  def canMoveLeft = {
    currentShape.layout.indices.forall(y => currentShape.layout(y).indices.forall(x => 
    (currentShape.x + x > 2 && !this.lockedImages.exists(tile => tile.x == currentShape.x + x - 1 && tile.y == currentShape.y + y)) || currentShape.layout(y)(x) == 0))
  }
  
  def canMoveRight = {
    currentShape.layout.indices.forall(y => currentShape.layout(y).indices.forall(x => 
    (currentShape.x + x < 11 && !this.lockedImages.exists(tile => tile.x == currentShape.x + x + 1 && tile.y == currentShape.y + y)) || currentShape.layout(y)(x) == 0))
  }
  def isLocked = {
    currentShape.layout.indices.exists(y => currentShape.layout(y).indices.exists(x => 
    (currentShape.layout(y)(x) == 1 && this.lockedImages.exists(tile => tile.x == currentShape.x + x && tile.y == currentShape.y + y + 1)) || isOnBoundary))
  }
  
  def isOnBoundary = {
    var res = false
    for(y <- currentShape.layout.indices) {
      for(x <- currentShape.layout(y).indices) {
        if(currentShape.layout(y)(x) == 1 && currentShape.y + y >= GridHeight - 1) {
          res = true
        }
      }
    }
    res
  }
  
  def hold() = { 
    val temp = currentShape
    holdShape match {
      case Some(shape) => currentShape = shape
      case None => currentShape = newShape()
    }
    currentShape.x = temp.x
    currentShape.y = temp.y
    holdShape = Some(temp)
  }
  
  def awardPoints = ???
  
  def deleteRows(y: Int) = {
    this.lockedImages.foreach(tile => 
      if(tile.y == y) {
        lockedImages = lockedImages.filterNot(_ == tile)
        lockedTiles(tile.y)(tile.x) = 1
      } else if(tile.y < y) tile.y += 1)
  }
  
  def lock() = {
    lockedShapes = lockedShapes ++: Vector(currentShape)
    for(y <- currentShape.layout.indices) {
      for(x <- currentShape.layout(y).indices) {
        if (currentShape.layout(y)(x) == 1) {
        lockedImages = lockedImages ++: Vector(new Tile(currentShape.image, currentShape.x + x, currentShape.y + y))
        lockedTiles(currentShape.y + y)(currentShape.x + x) = 1
//        lockedImages.indices.foreach(row => if (lockedImages(row).indices.forall(tile => 
//          lockedImages(row)(tile).isInstanceOf[Tile] || tile > 11 || tile < 2)) deleteRows(row)) 
        if(lockedImages.filter(_.y == lockedImages.last.y).size == 10) deleteRows(lockedImages.last.y)
        }
      }
    }
    currentShape = newShape()
  }
  
  def hasEnded = {
    lockedImages.exists(_.y <= 0)
  }
  
  
  
             
  
}