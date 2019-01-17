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
  
  
  val image = ImageIO.read(new File("src/spritesheet.png"))
  
  val cropped = image.getSubimage(0, 0, 32 * 17, 32 * 26)
  
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
  
  
  for(i <- 0 until image.getHeight() / 32) {
    for(j <- 0 until image.getWidth() / 32) {
      grid(i)(j) = image.getSubimage(j * 32, i * 32, 32, 32)
    }
  }
  
  var currentShape = newShape(Random.nextInt(7))
//  var nextShape = new Shape(this.layouts(Random.nextInt(7)), grid(0)(0))
  
  var points = 0
  
  

  def top = new MainFrame { 
    val pic = new Label { 
      icon = new ImageIcon(frame)
//      override def paintComponent(g: Graphics2D) = {
//        g.drawImage(image, 0, 0, null)
//      }
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
      
        case mouseClicked: MouseClicked => 
           println("Mouse clicked")
     
      }
      focusable = true
      requestFocus
    }
//    val g = image.createGraphics()
    var i = 1
    def startAnimating(interval: Int) = {
      val task = new TimerTask {
        def run() = {
          i += 1
          if(i % 20 == 0) currentShape.fall()
          g.drawImage(cropped, 32, 0, null)
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
//    currentShape = nextShape
    new Shape(this.layoutMap(layouts(r)), grid(r)(0))
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