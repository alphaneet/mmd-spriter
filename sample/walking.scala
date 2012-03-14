import processing.core._
import processing.core.PConstants._
import java.io.File
import java.awt.Rectangle

class Applet extends PApplet {
  applet =>

  class Sprite(
    path: String,
    ext: String,
    size: Int,
    reloopIndex: Int = 0
  ) extends Rectangle {
    private var index = 0
    
    private var _isFlip = false
    def isFlip = _isFlip
    def flip() {
      index = 0
      _isFlip = !_isFlip      
    }
    
    private lazy val leftImages: List[PImage] = {
      val list = List.range(0, size) map {
        i =>
        loadImage(path + "%04d.".format(i) + ext)
      }
      
      width  = list.head.width
      height = list.head.height
      list
    }

    private lazy val rightImages: List[PImage] = leftImages.map {
      src =>
      val (w, h) = (src.width, height)
      val dest = createImage(w, h, ARGB)
      for {
        lx <- (0 until w)
        ly <- (0 until h)
      } {
        val i = lx + (ly * w)
        val j = (w - lx -1) + (ly * w)        
        dest.pixels(i) = src.pixels(j)
      }
      dest
    }
    
    def images = if (isFlip) rightImages else leftImages
      
    def draw() {
      image(images(index), x, y, width, height)
    }

    def loop() = {
      index += 1
      
      if (index >= images.size) index = reloopIndex
    }
  }

  val backgroundImage = loadImage("background.png")
  
  class Haku extends Sprite("haku/haku_", "png", 61, 36) {
    private var _muki: Int = LEFT
    def muki = _muki
    def muki_=(muki: Int) {
      if (_muki != muki) flip()
      _muki = muki      
    }

    def move() {
      var tmp = x
      if (_muki == LEFT) tmp -= 8
      else tmp += 8
      if (tmp > 0 && tmp < applet.width - width) x = tmp
    }
  }
  
  val haku = new Haku()
  
  override def setup() {
    size(640, 480)
    frameRate(30)
    haku.muki = RIGHT
    haku.y = 300
  }

  var mode = 0
  override def draw() {
    mode match {
      case 0 => background(backgroundImage)
      case 1 => background(0)
      case 2 => background(255)
    }

    haku.move()
    haku.draw()
    haku.loop()
  }

  override def keyPressed() {
    List(LEFT, RIGHT) foreach {
      k =>
      if (k == keyCode ) {
        haku.muki = keyCode
      }
    }

    if (keyCode == ENTER) mode = (mode + 1) % 3      
  }
}

object Application extends Applet {
  def main(args: Array[String]) = runSketch()
}
