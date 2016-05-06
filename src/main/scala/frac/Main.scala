// modified by Hanns Holger Rutz in May 2016

/*
 * Copyright (C) 2012 Julien Letrouit
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 * http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package frac

import java.awt.event.KeyEvent
import java.awt.image.BufferedImage
import java.awt.{Color, Desktop, Font}
import java.net.URI
import javax.swing.KeyStroke

import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.concurrent.duration._
import scala.swing.BorderPanel.Position._
import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal

object Main extends SimpleSwingApplication {
  val TURTLE_MOVES_STAT_TEMPLATE    = "Turtle moves: %,d"
  val TURTLE_TURNS_STAT_TEMPLATE    = "Turtle turns: %,d"
  val SEQUENCE_LENGTH_STAT_TEMPLATE = "Sequence length: %,d"
  val DURATION_STAT_TEMPLATE        = "Drawing duration: %d ms"
  val CODE_COLOR                    = 80
  val parser                        = new FracDefParser
  val definitions: List[FracDef]    = ExampleRepository.examples
  var definition:       FracDef     = definitions.head

  val refreshAction = new Action("Refresh") {
    override def apply(): Unit = refresh()
    accelerator = Some(KeyStroke.getKeyStroke("F5"))
  }
  val increaseDepthAction = new Action("+") {
    override def apply(): Unit = increaseDepth()
    accelerator     = Some(KeyStroke.getKeyStroke("F6"))
    longDescription = "Increase depth"
  }
  val decreaseDepthAction = new Action("-") {
    override def apply(): Unit = decreaseDepth()
    accelerator     = Some(KeyStroke.getKeyStroke("F4"))
    longDescription = "Decrease depth"
  }

  val turtleMovesStat     = new Label
  val turtleTurnsStat     = new Label
  val sequenceLengthStat  = new Label
  val durationStat        = new Label

  val menu = new MenuBar {
    contents += new Menu("Load example") {
      definitions.foreach(ds => contents += new MenuItem(Action(ds.title) {
        selectExample(ds)
      }))
    }
    contents += new Menu("View") {
      contents += new MenuItem(refreshAction)
      contents += new MenuItem("Decrease depth") {
        reactions += {
          case ButtonClicked(_) => decreaseDepth()
        }
        peer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F4, 0))
      }
      contents += new MenuItem("Increase depth") {
        reactions += {
          case ButtonClicked(_) => increaseDepth()
        }
        peer.setAccelerator(KeyStroke.getKeyStroke(KeyEvent.VK_F6, 0))
      }
    }
    contents += new Menu("Help") {
      contents += new MenuItem(Action("User manual") { browse("https://github.com/jletroui/frac/blob/master/README.markdown") })
      contents += new MenuItem(Action("License"    ) { browse("https://raw.github.com/jletroui/frac/master/LICENSE") })
    }
  }
  val editor = new TextArea(definitions.head.sourceText, 5, 20) {
    font        = new Font("Monospaced", Font.BOLD, 16)
    foreground  = new Color(CODE_COLOR, CODE_COLOR, CODE_COLOR)
    border      = TitledBorder(null, "Editor")
  }
  val depth = new TextField("1", 3) {
    maximumSize = preferredSize
    verifier    = (txt: String) => try { txt.toInt ; true} catch { case NonFatal(t) => false }
  }
  object fractalPanel extends Panel {
    def width : Int = peer.getWidth
    def height: Int = peer.getHeight

    override def paintComponent(g: Graphics2D): Unit = {
      super.paintComponent(g)
      val w = width
      val h = height
      if (image.getWidth == w && image.getHeight == h)
        g.drawImage(image, 0, 0, peer)
      else
        refresh()
    }
  }

  val rightSection = new BoxPanel(Orientation.Vertical) {
    contents += new ScrollPane(editor)
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += new BoxPanel(Orientation.Vertical) {
        contents += turtleMovesStat
        contents += turtleTurnsStat
        contents += sequenceLengthStat
        contents += durationStat
      }
      contents += HGlue

      border = TitledBorder(null, "Drawing stats")
    }
    contents += new BoxPanel(Orientation.Horizontal) {
      contents += HGlue
      contents += new Label("Depth: ")
      contents += new Button(decreaseDepthAction)
      contents += depth
      contents += new Button(increaseDepthAction)
      contents += HStrut(10)
      contents += new Button(refreshAction)
    }
  }

  val center = new SplitPane(Orientation.Vertical, fractalPanel, rightSection) {
    continuousLayout    = true
    oneTouchExpandable  = true
    dividerLocation     = 1200
  }

  lazy val topFrame = new MainFrame {
    title = "Frac 1.0.5"
    contents = new BorderPanel {
      preferredSize   = (1600,1000)
      opaque          = true
      layout(center)  = Center
    }
    menuBar = menu
    centerOnScreen()
    listenTo(this)
    reactions += {
      case WindowClosing(e) =>
        println("Exiting...")
        System.exit(0)
    }
  }

  def top = topFrame

  def increaseDepth(): Unit = {
    depth.text = (depth.text.toInt + 1).toString
    refresh()
  }

  def decreaseDepth(): Unit = {
    val before = depth.text.toInt
    if (before > 0) {
      depth.text = (before - 1).toString
      refresh()
    }
  }

  def selectExample(example: FracDef): Unit = {
    editor.text = example.sourceText
    depth.text = "1"
    refresh()
  }

  private var image     = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
  private var renderer  = Option.empty[GraphicsRenderer]
  private var rendering = Future.failed[RendererStats](new Exception("Not yet started"))

  def refresh(): Unit =
    try {
      val parsingResult = parser.parseFracDef(editor.text)

      if (parsingResult.matched) {
        definition = parsingResult.result.get
        val w       = fractalPanel.width
        val h       = fractalPanel.height
        renderer.foreach(_.isRunning = false) // "abort"
        Await.ready(rendering, 100.milli)
        if (image.getWidth != w || image.getHeight != h) {
          image.flush()
          image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
        }
        val g = image.createGraphics()
        g.setBackground(new Color(255, 255, 255, 0))
        g.clearRect(0,0, w, h)
        g.setColor(new Color(100,100,100))
        val r     = new GraphicsRenderer(g, w, h)
        renderer  = Some(r)
        val df    = definition
        val dp    = depth.text.toInt
        import ExecutionContext.Implicits.global
        val res   = Future(blocking(r.render(df, dp)))
        rendering = res
        res.foreach { stats =>
          onEDT {
            if (rendering == res) {
              turtleMovesStat   .text = TURTLE_MOVES_STAT_TEMPLATE    .format(stats.turtleMoves)
              turtleTurnsStat   .text = TURTLE_TURNS_STAT_TEMPLATE    .format(stats.turtleTurns)
              sequenceLengthStat.text = SEQUENCE_LENGTH_STAT_TEMPLATE .format(stats.sequenceLength)
              durationStat      .text = DURATION_STAT_TEMPLATE        .format(stats.duration)
              fractalPanel.repaint()
            }
          }
        }
      }
      else {
        val error = parsingResult.parseErrors.head
        val message = new StringBuilder("There is a syntax error at the character %d.".format(error.getStartIndex))
        if (error.getErrorMessage != null) message.append(" Additional info: %s".format(error.getErrorMessage))
        Dialog.showMessage(
          message = message,
          title = "Syntax error",
          messageType = Dialog.Message.Error)

        editor.caret.position = error.getStartIndex
        editor.requestFocusInWindow()
      }
    }
    catch {
      case NonFatal(t) =>
        Dialog.showMessage(message = t.getMessage, title = "Syntax error", messageType = Dialog.Message.Error)
    }

  lazy val desktop = if (Desktop.isDesktopSupported) Some(Desktop.getDesktop) else None
  lazy val browser = if (desktop.isDefined && desktop.get.isSupported(Desktop.Action.BROWSE)) Some(desktop.get.browse _) else None

  def browse(url: String): Unit = browser.foreach(_(new URI(url)))
}