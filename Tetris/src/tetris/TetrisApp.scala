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
import java.awt.Font

object TetrisApp extends SimpleSwingApplication {
  
  
  val image = ImageIO.read(new File("tetris/src/spritesheet.png"))
  
  val TileSize = 32
  
  val GridHeight = 23
  
  val GridWidth = 10
  
  var level = 1
  
  private var score = 0
  
  val cropped = image.getSubimage(32, 0, TileSize * 17, TileSize * 26)
  
  def emptyImage(w: Int, h: Int) = {
    new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  }
  
  val gameOverImage = image.getSubimage(0, TileSize * GridHeight, TileSize * 9, TileSize * 2)
  
  val coverImage = image.getSubimage(TileSize * 10, TileSize * GridHeight, TileSize * 9, TileSize * 2)
  
  val frame = this.emptyImage(cropped.getWidth(), cropped.getHeight()) // TODO
  val g = frame.createGraphics()
  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g.setFont(new Font("Tetris Font", 0, 30))
  
  
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
 
  
  var currentShape = newShape()
  
  var nextShape = newShape()

  var holdShape: Option[Shape] = None
  

  //Creates the app window and the background on which the graphics will be drawn
  def top = new MainFrame { 
    val pic = new Label { 
      icon = new ImageIcon(frame)
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += pic
      listenTo(keys, mouse.clicks)
      
      //Event listeners for game controls 
      reactions += {
        case KeyPressed(_, Key.A, _, _) => 
            currentShape.moveLeft()
            currentShape.show(g)
        
        case KeyPressed(_, Key.D, _, _) => 
            currentShape.moveRight()
            currentShape.show(g)
        case KeyPressed(_, Key.W, _, _) =>
            currentShape.rotate()
            currentShape.show(g)
        case KeyPressed(_, Key.S, _, _) =>
            fallInterval = 2
        case KeyReleased(_, Key.S, _, _) => 
            fallInterval = 48
        case KeyPressed(_, Key.Q, _, _) =>
            hold()
        case KeyPressed(_, Key.Space, _, _) => 
            while(!currentShape.isLocked) {
              currentShape.fall()
            }
      
      }
      focusable = true
      requestFocus
    }
    
    val timer = new Timer()
    var fallInterval = 48
    var i = 1
    def startAnimating(interval: Int) = {
      val task = new TimerTask {
        def run() = {
          if(hasEnded) timer.cancel()
          i += 1
          if(currentShape.isLocked) lock()
          if(i % fallInterval == 0) currentShape.fall()
          g.drawImage(cropped, TileSize, 0, null)
          g.translate((TileSize * (8.5 - nextShape.centerPoint._1)).toInt, (TileSize * (3.5 - nextShape.centerPoint._2)).toInt)
          nextShape.show(g)
          g.translate((-TileSize * (8.5 - nextShape.centerPoint._1)).toInt, (-TileSize * (3.5 - nextShape.centerPoint._2)).toInt)
          drawLockedTiles()
          g.drawImage(coverImage, TileSize * 9, TileSize * GridHeight, null)
          g.drawString(score.toString, 32 * 14 - score.toString.length * 8, 32 * 9)
          currentShape.show(g)
          pic.repaint()
        }
      }
      timer.schedule(task,0,interval)
    }
    
    startAnimating(32)
  }

 
  var time = 0
  
  def newShape() = {
    val r = Random.nextInt(7)
    new Shape(this.layoutMap(layouts(r)), image.getSubimage(0, r * TileSize, TileSize, TileSize))
  }
  
  var lockedTiles = Array.ofDim[Int](image.getHeight(), image.getWidth())
  
  var lockedImages = Vector[Tile]()
  
  
  //Draws all the tiles that have been played and haven't been removed
  def drawLockedTiles() = {
    lockedImages.foreach(tile => if(tile.isInstanceOf[Tile]) tile.show(g))
  }
  
  
  
  //Holds the current block and gives the player the block that is 
  //currently in hold or a new shape if there's not a block in hold yet.
  def hold() = { 
    val temp = currentShape
    holdShape match {
      case Some(shape) => currentShape = shape
      case None => currentShape = newShape()
    }
    currentShape.x = math.max(1 + temp.leftSide - currentShape.leftSide, math.min(temp.x, temp.x + temp.rightSide - currentShape.rightSide))
    currentShape.y = temp.y
    holdShape = Some(temp)
  }
  
  //Gives the player points for removing rows.
  def awardPoints(n: Int) = {
    val points = n match {
      case 1 => 40
      case 2 => 100 
      case 3 => 300
      case 4 => 1200
    }
    this.score += points * level
  }
  
  
  //Removes a row at a given height and moves the rows above the removed row down.
  def deleteRows(y: Int) = {
    this.lockedImages.foreach{ tile => 
      if(tile.y == y) {
        lockedImages = lockedImages.filterNot(_ == tile)
        lockedTiles(tile.y)(tile.x) = 1
      } else if(tile.y < y) tile.y += 1
    }
  }
  
  //Locks the current block and activates the next one.
  def lock() = {
    var rowsRemoved = 0
    currentShape.layout.indices.foreach{ y => currentShape.layout(y).indices.foreach {x => 
      if (currentShape.layout(y)(x) == 1) {
        lockedImages = lockedImages ++: Vector(new Tile(currentShape.image, currentShape.x + x, currentShape.y + y))
        lockedTiles(currentShape.y + y)(currentShape.x + x) = 1
        if(lockedImages.filter(_.y == lockedImages.last.y).size == 10) {
          rowsRemoved += 1
          deleteRows(lockedImages.last.y)
        }
      }
    }}
    if(rowsRemoved != 0) awardPoints(rowsRemoved)
    currentShape = nextShape
    nextShape = newShape()
  }
  
  //Determines whether there is a block that has a tile 
  //touching the ceiling i.e. whether the game has ended
  def hasEnded = {
    lockedImages.exists(_.y <= 0)
  }
  
  
  
             
  
}