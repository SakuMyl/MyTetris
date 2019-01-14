package tetris

import swing._
import java.io.File
import javax.imageio.ImageIO
import java.awt.image.BufferedImage
import javax.swing.ImageIcon
import swing.event._
import java.util.{Timer, TimerTask}
import java.awt.{Graphics2D, Color}
import java.awt.geom._

object TetrisApp extends SimpleSwingApplication {
  
  
  val image = ImageIO.read(new File("src/spritesheet.png")).getSubimage(0, 0, 32 * 17, 32 * 26)
  
  
  
  
  val layouts: Map[String, Array[Array[Int]]] = {
    
     Map("s" -> Array(Array(1,0,0),
                      Array(1,1,0),
                      Array(0,1,0)),
                      
         "z" -> Array(Array(0,1,0),
                      Array(1,1,0),
                      Array(1,0,0)),
                      
         "j" -> Array(Array(0,1,0),
                      Array(0,1,0),
                      Array(1,1,0)),
                      
         "l" -> Array(Array(0,1,0),
                      Array(0,1,0),
                      Array(0,1,1)),
                      
         "t" -> Array(Array(0,0,0),
                      Array(1,1,1),
                      Array(0,1,0)),
                      
         "o" -> Array(Array(1,1),
                      Array(1,1)),
                      
         "i" -> Array(Array(0,1,0,0),
                      Array(0,1,0,0),
                      Array(0,1,0,0),
                      Array(0,1,0,0))
        )
              
  }
  
  
  
  val grid = Array.ofDim[BufferedImage](17, 26)
  
  for(i <- 0 until 17) {
    for(j <- 0 until 26) {
      grid(i)(j) = image.getSubimage(i * 32, j * 32, 32, 32)
    }
  }
  
  var currentShape = new Shape(this.layouts("i"), grid(0)(0))
  var nextShape = new Shape(this.layouts("l"), grid(0)(0))
  
  var points = 0
  
  def top = new MainFrame { 
    val pic = new Label { 
      icon = new ImageIcon(image)
//      override def paintComponent(g: Graphics2D) = {
//        g.drawImage(grid(0)(0), 0, 0, null)
//      }
    }
    contents = new BoxPanel(Orientation.Vertical) {
      contents += pic
    }
    this.reactions += {
      case KeyPressed(_, Key.A, _, _) => 
          currentShape.moveLeft()
          println("A pressed")
        
      case KeyPressed(_, Key.D, _, _) => 
          currentShape.moveRight()
          println("D pressed!")
      
      case mouseClicked: MouseClicked => 
         println("Mouse clicked")
     
    }
    listenTo(pic.mouse.clicks, pic.keys)
    val g = image.createGraphics()
    def startAnimating(interval: Int) = {
      val task = new TimerTask {
        def run() = {
          currentShape.fall()
          currentShape.show(g)
          pic.repaint()
        }
      }
      new Timer().schedule(task,0,interval)
    }
    
    startAnimating(1000)
  }

 
  var time = 0
  
  def newShape() = {
    currentShape = nextShape
    nextShape = new Shape(this.layouts("j"), grid(0)(0))
  }
//  
//  var lockedTiles = ???
//  
//  def draw() = ???
//  
//  def show = ??? 
//  
//  def hold = ???
//  
//  def awardPoints = ???
//  
//  def lock = ??? 
//  
//  def hasEnded = ???
//  
//  
  
             
  
}