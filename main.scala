import processing.core._
import processing.core.PConstants._
import java.io.File

object ImageMagickCommand {
  
  def apply(
    imageMagickPath: String,
    transparentColor: String,
    xoffset: Int,
    yoffset: Int,
    inputWidth: Int,
    inputHeight: Int,
    outputWidth: Int,
    outputHeight: Int,
    tmpFilename: String,
    inputFilename: String,
    outputFilename: String
  ) {
    List(
      "-transparent %s %s %s".format(
        transparentColor,
        inputFilename,
        tmpFilename
      ),
      "-crop %dx%d+%d+%d %s %s".format(
        inputWidth,
        inputHeight,
        xoffset,
        yoffset,
        tmpFilename,
        tmpFilename
      ),      
      "-geometry %dx%d %s %s".format(      
        outputWidth,
        outputHeight,
        tmpFilename,
        tmpFilename
      ),    
      "%s %s".format(tmpFilename, outputFilename)
    ) foreach {
      command =>
      scala.sys.process.Process(imageMagickPath + "convert " + command) !
    }

    (new File(tmpFilename)).delete()
  }  
}

trait Config {
  private val elem: scala.xml.Elem = try {
    scala.xml.XML.loadFile("config.xml")
  } catch {
    case ex =>
    javax.swing.JOptionPane.showMessageDialog(null, ex.toString)
    <dummy></dummy>
  }    
  
  def apply[T](symbol: Symbol, default: T): T = {
    try {
      val text = (elem \ symbol.name).text
      val v = default match {
        case _: Int => text.toInt
        case _: String => if (text.isEmpty) default else text
      }
      v.asInstanceOf[T]
    } catch {        
      case _ => default
    }      
  }

  val imageMagickPath = apply('imageMagickPath, "")
  
  val importPath = apply('importPath, "./")
  val exportPath = apply('exportPath, "export/")

  val importExt  = apply('importExt, "bmp")
  val exportExt  = apply('exportExt, "png")
  
  val exportSize = apply('exportSize, 128)
}
  
class Applet extends PApplet with Config {
  applet =>

  var index = 0
  var img: Option[PImage] = None
  var filename = ""
  
  private var _mode: Mode = Import
  def mode = _mode
  def mode_=(mode: Mode) {
    _mode = mode
    cursor(mode.cursorType)
  }  

  trait Mode {    
    val path: String

    val ext: String
    
    def title: String    

    def cursorType: Int

    def filenames: List[String] = {
      val f = new File(path)
      if (f.exists) f.list.filter { _ endsWith ext } toList else Nil
    }  

    def draw() {}

    def mousePressed() {}

    def mouseReleased() {}

    def mouseMoved() {}

    def mouseDragged() {}  
    
    def keyPressed() {}
  }

  object Export extends Mode {
    val path = exportPath
    val ext  = exportExt
    
    def title = "viewer"

    def cursorType = ARROW
  }
  
  object Import extends Mode {
    thismode =>
    val path = importPath
    val ext  = importExt

    def title = img map {
      i =>
      "%s: %dx%d".format(filename, i.width, i.height)
    } getOrElse("Not Found Image")

    def cursorType = if (drag.rect.contains(mouseX, mouseY)) HAND else ARROW
    
    var action: Action = Select

    sealed abstract case class Action(id: Int) {
      def mousePressed(): Unit
      def mouseDragged(): Unit
    }

    object Select extends Action(0) {
      def mousePressed() {
        drag.bx = mouseX
        drag.by = mouseY
        drag.ax = mouseX
        drag.ay = mouseY

        redraw()
      }

      def mouseDragged() {
        drag.ax = mouseX
        drag.ay = mouseY        
      }
    }
    
    object Move extends Action(1) {
      def mousePressed() {}
      def mouseDragged() { drag.move(mouseX - pmouseX, mouseY - pmouseY) }
    }

    // 後で実装する（しないフラグ）
    object Resize extends Action(2) {
      def mousePressed() {}
      def mouseDragged() {}
    }     

    override def draw() {
      noFill()
      stroke(255, 255, 0)
      strokeWeight(2)
      rect(drag.x, drag.y, drag.width, drag.height)

      fill(255)
      val s = "x=%d, y=%d, w=%d, h=%d".format(drag.x, drag.y, drag.width, drag.height)
      text(s, 5, 20)
    }
    
    override def mousePressed() {
      action = if (cursorType == HAND) Move else Select
      action.mousePressed()
    }

    override def mouseMoved() = cursor(cursorType)
    
    override def mouseDragged() {
      action.mouseDragged()
      redraw()
    }  

    override def keyPressed() {
      def isKey(c: Char) = (key == c.toLower || key == c.toUpper)
        
      Map(
        'a' -> (-1,  0),
        'd' -> ( 1,  0),
        'w' -> ( 0, -1),
        's' -> ( 0,  1)
      ) foreach {
        case (c, (x, y)) => if (isKey(c)) {
          drag.move(x, y)
          redraw()
        }
      }

      Map(
        'j' -> (-1,  0), 
        'k' -> ( 0,  1),
        'l' -> ( 1,  0),
        'i' -> ( 0, -1) 
      ) foreach {
        case (c, (x, y)) => if (isKey(c)) {
          drag.ax += x
          drag.ay += y
          redraw()
        }
      }      
        
      if (keyCode == ENTER) {
        try {
          val export = new File(Export.path)
          export.mkdir()
          export.listFiles() foreach { _.delete() }
          
          thismode.filenames foreach {
            filename =>
              
            val ext = filename.split("\\.").last
            val tmp = Export.path + filename + "." + ext
            val input = thismode.path + filename
            val output = Export.path + filename.replaceAll(ext + "$", Export.ext)

            val color: String = "#" + {
              val c = loadImage(input).pixels(0)

              ("" /: List(red _, green _, blue _)) {
                (s, f) =>
                  val v = Integer.toHexString(f(c).toInt)
                s + (if (v.size == 1.toInt) "0" + v else v)              
              }
            }

            ImageMagickCommand(
              imageMagickPath  = imageMagickPath,
              transparentColor = color,
              xoffset          = drag.x,
              yoffset          = drag.y,
              inputWidth       = drag.width,
              inputHeight      = drag.height,
              outputWidth      = exportSize,
              outputHeight     = exportSize,
              tmpFilename      = tmp,
              inputFilename    = input,
              outputFilename   = output
            )
          }
        } catch {
          case ex =>
          javax.swing.JOptionPane.showMessageDialog(null, ex.toString)            
        }
        
        applet.mode = Export
        reload()        
      }
    }
  }

  object drag {
    var (bx, by) = (0, 0) // before drag location
    var (ax, ay) = (0, 0) // after  drag location

    def x = math.min(bx, ax)
    def y = math.min(by, ay)

    def width  = math.abs(bx - ax)
    def height = math.abs(by - ay)

    def rect = new java.awt.Rectangle(x, y, width, height)

    def move(x: Int, y: Int) {
      bx += x
      by += y

      ax += x
      ay += y
    }    
  }

  def title: String = if (frame != null) frame.getTitle else ""
  def title_=(title: String) = if (frame != null) frame.setTitle(title)
  
  def reload() {
    val filenames = mode.filenames

    this.img = if (filenames.isEmpty) {
      None
    } else {
      val s = filenames.size
    
      index = (index + s) % s

      this.filename = filenames(index)

      val i = loadImage(mode.path + "/" + filename)
      val w = i.width
      val h = i.height
      
      frame.setSize(w, h)
      size(w, h)
    
      Option(i)
    }

    this.title = mode.title

    redraw()
  }
  
  override def setup() {
    size(800, 800)

    smooth()

    textFont(createFont("", 18))

    noLoop()
    
    reload()  
  }

  override def draw() {
    background(255)
    
    img foreach { image(_, 0, 0) }
      
    mode.draw()
  }

  override def mousePressed() = mode.mousePressed()

  override def mouseDragged() = mode.mouseDragged()

  override def mouseMoved() = mode.mouseMoved()  

  override def keyPressed() {
    mode.keyPressed()

    Map(LEFT -> -1, RIGHT -> 1) find {
      _._1 == keyCode
    } foreach {
      v =>
      index += v._2
      reload()
    }
  
    if (key == 'x' || key == 'X') {
      this.mode = if (mode == Import) Export else Import
      reload()
    }
  }
}

object Application extends Applet {
  import javax.swing.JOptionPane
  def main(args: Array[String]) = runSketch()
}
