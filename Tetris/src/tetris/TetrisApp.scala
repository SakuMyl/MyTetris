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
  
  def level = 1 + rowsRemoved / 10
  
  var rowsRemoved = 0
  
  
  
  def fallingSpeed = {
    if(level <= 9) {
      48 - level * 5
    } else if(level == 10) {
      6
    } else if(level <= 13) {
      5
    } else if(level <= 16) {
      4
    } else if(level <= 19) {
      3
    } else if(level <= 29) {
      2
    } else {
      1
    }
  }
  
  private var score = 0
  
  val cropped = image.getSubimage(32, 0, TileSize * 17, TileSize * 23)
  val withGameOver = image.getSubimage(32, 0, TileSize * 17, TileSize * 26)
  
  def emptyImage(w: Int, h: Int) = {
    new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
  }
  
  val gameOverImage = image.getSubimage(0, TileSize * GridHeight, TileSize * 9, TileSize * 2)
  
  val coverImage = image.getSubimage(TileSize * 10, TileSize * GridHeight, TileSize * 9, TileSize * 2)
  
  val frame = this.emptyImage(withGameOver.getWidth(), withGameOver.getHeight()) // TODO
  val g = frame.createGraphics()
  g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
  g.setFont(new Font("Tetris Font", 10, 30))
  
  
  val layoutMap: Map[String, Array[Array[Int]]] = {
    
     Map("j" -> Array(Array(1,0,0),
                      Array(1,1,1),
                      Array(0,0,0)),
                      
         "i" -> Array(Array(0,0,0,0),
                      Array(1,1,1,1),
                      Array(0,0,0,0),
                      Array(0,0,0,0)),
                      
         "s" -> Array(Array(0,1,1),
                      Array(1,1,0),
                      Array(0,0,0)),
                      
         "o" -> Array(Array(1,1),
                      Array(1,1)),
                      
         "l" -> Array(Array(0,0,1),
                      Array(1,1,1),
                      Array(0,0,0)),
                      
         "z" -> Array(Array(1,1,0),
                      Array(0,1,1),
                      Array(0,0,0)),
                      
         "t" -> Array(Array(0,1,0),
                      Array(1,1,1),
                      Array(0,0,0))
         
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
            fallInterval = 1
        case KeyReleased(_, Key.S, _, _) => 
            fallInterval = fallingSpeed
        case KeyPressed(_, Key.Q, _, _) =>
            hold()
        case KeyPressed(_, Key.Space, _, _) => 
            while(!currentShape.isLocked) {
              currentShape.fall()
            }
            lock()
        case KeyPressed(_, _, _, _) => 
            if(hasEnded) {
              restart()
              timer = new Timer()
              startAnimating(32)
            }
      
      }
      focusable = true
      requestFocus
    }
    
    var timer = new Timer()
    var fallInterval = fallingSpeed
    var i = 1
    def startAnimating(interval: Int) = {
      val task = new TimerTask {
        def run() = {
          if(hasEnded) {
            g.drawImage(gameOverImage, TileSize, GridHeight * TileSize, null)
            timer.cancel()
          } else {
            g.drawImage(cropped, TileSize, 0, null)
            currentShape.show(g)
          }
          i += 1
          if(i % fallInterval == 0) {
            if(currentShape.isLocked) lock()
            else currentShape.fall()
          }
          g.translate((TileSize * (8.5 - nextShape.centerPoint._1)).toInt, (TileSize * (3.5 - nextShape.centerPoint._2)).toInt)
          nextShape.show(g)
          g.translate((-TileSize * (8.5 - nextShape.centerPoint._1)).toInt, (-TileSize * (3.5 - nextShape.centerPoint._2)).toInt)
          drawLockedTiles()
          g.drawString(score.toString, 32 * 14 - score.toString.length * 8, 32 * 9)
          g.drawString(level.toString, 32 * 14 - level.toString.length * 8, 32 * 14)
          pic.repaint()
        }
      }
      timer.schedule(task,0,interval)
    }
    
    startAnimating(32)
  }

 
  
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
    var rows = 0
    currentShape.layout.indices.foreach{ y => currentShape.layout(y).indices.foreach {x => 
      if (currentShape.layout(y)(x) == 1) {
        lockedImages = lockedImages ++: Vector(new Tile(currentShape.image, currentShape.x + x, currentShape.y + y))
        lockedTiles(currentShape.y + y)(currentShape.x + x) = 1
        if(lockedImages.filter(_.y == lockedImages.last.y).size == 10) {
          rows += 1
          deleteRows(lockedImages.last.y)
        }
      }
    }}
    if(rows != 0) awardPoints(rows)
    this.rowsRemoved += rows
    currentShape = nextShape
    if(lockedImages.exists(tile => tile.y <= 1 && (tile.x <= 5 + currentShape.layout.size || tile.x >= 5))) {
      currentShape.y = -1
    }
    nextShape = newShape()
  }
  
  //Determines whether there is a block that has a tile 
  //touching the ceiling i.e. whether the game has ended
  def hasEnded = {
    lockedImages.exists(tile => tile.y <= 0 && (tile.x <= 8 || tile.x >= 5))
  }
  
  def restart() = {
    this.lockedTiles = Array.ofDim[Int](image.getHeight(), image.getWidth())
    this.lockedImages = Vector[Tile]()
    this.score = 0
    this.rowsRemoved = 0
    this.currentShape = newShape()
    this.nextShape = newShape()
    this.holdShape = None
  }
  
  
             
  
}