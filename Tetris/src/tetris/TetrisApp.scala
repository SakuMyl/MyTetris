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
  
  var currentShape = newShape(Random.nextInt(7))
  var nextShape = newShape(Random.nextInt(7))
  
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
            currentShape.moveLeft()
            currentShape.show(g)
        
        case KeyPressed(_, Key.D, _, _) => 
            currentShape.moveRight()
            currentShape.show(g)
        case KeyPressed(_, Key.E, _, _) =>
            currentShape.rotate()
            currentShape.show(g)
        case KeyPressed(_, Key.S, _, _) =>
            fallInterval = 1
        case KeyReleased(_, Key.S, _, _) => 
            fallInterval = 20
      
      }
      focusable = true
      requestFocus
    }
    
    var fallInterval = 20
    var i = 1
    def startAnimating(interval: Int) = {
      val task = new TimerTask {
        def run() = {
          i += 1
          if(isLocked) lock()
          if(i % fallInterval == 0) currentShape.fall()
          g.drawImage(cropped, TileSize, 0, null)
          drawLockedTiles()
          currentShape.show(g)
          pic.repaint()
        }
      }
      new Timer().schedule(task,0,interval)
    }
    
    startAnimating(30)
  }

 
  var time = 0
  
  def newShape(r: Int) = {
    new Shape(this.layoutMap(layouts(r)), grid(r)(0))
  }
  
  var lockedTiles = Array.ofDim[Int](image.getHeight(), image.getWidth())
  
  
  def drawLockedTiles() = {
    lockedTiles.indices.foreach(y => lockedTiles(y).indices.foreach(x => 
    if(lockedTiles(y)(x) == 1) g.drawImage(currentShape.image, TileSize * x, TileSize * y, null)))
  }
  
  def isLocked = {
    var res = false
    currentShape.layout.indices.foreach(y => currentShape.layout(y).indices.foreach(x => 
    if(currentShape.layout(y)(x) == 1 && this.lockedTiles(currentShape.y + y + 1)(currentShape.x + x) == 1 || isOnBoundary) res = true))
    res
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
//  def draw() = ???
//  
//  def show = ??? 
//  
//  def hold = ???
//  
//  def awardPoints = ???
//  
  def lock() = {
    for(y <- currentShape.layout.indices) {
      for(x <- currentShape.layout(y).indices) {
        if (currentShape.layout(y)(x) == 1) {
        lockedTiles(currentShape.y + y)(currentShape.x + x) = 1
        }
      }
    }
    currentShape = nextShape
    nextShape = newShape(Random.nextInt(7))
  }
  
//  def hasEnded = ???
//  
//  
  
             
  
}