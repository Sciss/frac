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
import java.awt.{Color, Desktop, FileDialog, Font, RenderingHints, Toolkit}
import java.net.URI
import javax.imageio.ImageIO
import javax.swing.{KeyStroke, SpinnerNumberModel}

import de.sciss.file._
import de.sciss.swingplus.{GroupPanel, Spinner}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future, blocking}
import scala.swing.BorderPanel.Position._
import scala.swing.Swing._
import scala.swing._
import scala.swing.event._
import scala.util.control.NonFatal

object Main extends SimpleSwingApplication {
  val TURTLE_MOVES_STAT_TEMPLATE = "Turtle moves: %,d"
  val TURTLE_TURNS_STAT_TEMPLATE = "Turtle turns: %,d"
  val SEQUENCE_LENGTH_STAT_TEMPLATE = "Sequence length: %,d"
  val DURATION_STAT_TEMPLATE = "Drawing duration: %d ms"
  val CODE_COLOR = 80
  val parser = new FracDefParser
  val definitions: List[FracDef] = ExampleRepository.examples
  var definition: FracDef = definitions.head

  private val menuMod = Toolkit.getDefaultToolkit.getMenuShortcutKeyMask

  val refreshAction = new Action("Refresh") {
    override def apply(): Unit = refresh()

    accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_R, menuMod))
  }
  val increaseDepthAction = new Action("+") {
    override def apply(): Unit = increaseDepth()

    accelerator = Some(KeyStroke.getKeyStroke("F6"))
    longDescription = "Increase depth"
  }
  val decreaseDepthAction = new Action("-") {
    override def apply(): Unit = decreaseDepth()

    accelerator = Some(KeyStroke.getKeyStroke("F4"))
    longDescription = "Decrease depth"
  }

  val turtleMovesStat = new Label
  val turtleTurnsStat = new Label
  val sequenceLengthStat = new Label
  val durationStat = new Label

  val menu = new MenuBar {
    contents += new Menu("Example") {
      definitions.foreach(ds => contents += new MenuItem(Action(ds.title) {
        selectExample(ds)
      }))
    }
    contents += new Menu("View") {
      contents += new MenuItem(refreshAction)
      contents += new MenuItem(new Action("Decrease depth") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_DOWN, menuMod))
        def apply(): Unit = decreaseDepth()
      })
      contents += new MenuItem(new Action("Increase depth") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_UP, menuMod))
        def apply(): Unit = increaseDepth()
      })
      contents += new MenuItem(new Action("Export Image...") {
        accelerator = Some(KeyStroke.getKeyStroke(KeyEvent.VK_S, menuMod))
        def apply(): Unit = exportImage()
      })
    }
    contents += new Menu("Help") {
      contents += new MenuItem(Action("User manual") {
        browse("https://github.com/jletroui/frac/blob/master/README.markdown")
      })
      contents += new MenuItem(Action("License") {
        browse("https://raw.github.com/jletroui/frac/master/LICENSE")
      })
    }
  }
  val editor = new TextArea(definitions.head.sourceText, 5, 20) {
    font        = new Font("Monospaced", Font.BOLD, 16)
    foreground  = new Color(CODE_COLOR, CODE_COLOR, CODE_COLOR)
    border      = TitledBorder(null, "Editor")
  }

  val mDepth = new SpinnerNumberModel(1, 1, 256, 1)

  val depth = new Spinner(mDepth)

  object fractalPanel extends Panel {
    def width: Int = peer.getWidth

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
      contents += depth
      contents += HStrut(10)
      contents += new Button(refreshAction)
      maximumSize = preferredSize
    }
  }

  val center = new SplitPane(Orientation.Vertical, fractalPanel, rightSection) {
    continuousLayout = true
    oneTouchExpandable = true
    dividerLocation = 1200
  }

  lazy val topFrame = new MainFrame {
    title = "Frac 1.0.5"
    contents = new BorderPanel {
      preferredSize = (1600, 1000)
      opaque = true
      layout(center) = Center
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

  def increaseDepth(): Unit = Option(mDepth.getNextValue    ).foreach(mDepth.setValue)
  def decreaseDepth(): Unit = Option(mDepth.getPreviousValue).foreach(mDepth.setValue)

  def selectExample(example: FracDef): Unit = {
    editor.text = example.sourceText
    mDepth.setValue(1)
    refresh()
  }

  def depthValue: Int = mDepth.getNumber.intValue()

  private var image = new BufferedImage(1, 1, BufferedImage.TYPE_INT_ARGB)
  private var renderer = Option.empty[GraphicsRenderer]
  private var rendering = Future.failed[RendererStats](new Exception("Not yet started"))

  private def prepareRenderer(img: BufferedImage, margin: Int = 20): GraphicsRenderer = {
    val g = img.createGraphics()
    g.setBackground(new Color(255, 255, 255, 0))
    val w = img.getWidth
    val h = img.getHeight
    g.clearRect(0, 0, w, h)
    g.setColor(new Color(100, 100, 100))
    g.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON)
    new GraphicsRenderer(g, w, h, MARGIN = margin)
  }

  def refresh(): Unit =
    try {
      val parsingResult = parser.parseFracDef(editor.text)

      if (parsingResult.matched) {
        definition = parsingResult.result.get
        val w = fractalPanel.width
        val h = fractalPanel.height
        renderer.foreach(_.isRunning = false) // "abort"
        Await.ready(rendering, 100.milli)
        if (image.getWidth != w || image.getHeight != h) {
          image.flush()
          image = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
        }
        val r = prepareRenderer(image)
        renderer = Some(r)
        val df = definition
        val dp = depthValue
        import ExecutionContext.Implicits.global
        val res = Future {
          blocking {
            val res = r.render(df, dp)
            r.g.dispose()
            res
          }
        }
        rendering = res
        res.foreach { stats =>
          onEDT {
            if (rendering == res) {
              turtleMovesStat.text = TURTLE_MOVES_STAT_TEMPLATE.format(stats.turtleMoves)
              turtleTurnsStat.text = TURTLE_TURNS_STAT_TEMPLATE.format(stats.turtleTurns)
              sequenceLengthStat.text = SEQUENCE_LENGTH_STAT_TEMPLATE.format(stats.sequenceLength)
              durationStat.text = DURATION_STAT_TEMPLATE.format(stats.duration)
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
  lazy val browser = if (desktop.isDefined && desktop.get.isSupported(Desktop.Action.BROWSE))
    Some(desktop.get.browse _) else None

  def browse(url: String): Unit = browser.foreach(_ (new URI(url)))

  private var lastExportDir = {
    val pic = userHome / "Pictures"
    if (pic.isDirectory) pic else userHome
  }

  private lazy val mExpWidth  = new SpinnerNumberModel(image.getWidth , 1, 32768, 1)
  private lazy val mExpHeight = new SpinnerNumberModel(image.getHeight, 1, 32768, 1)
  private lazy val mExpMargin = new SpinnerNumberModel(20, 0, 32768, 1)

  def exportImage(): Unit = {
    val title = "Export Image"
    val dlg = new FileDialog(topFrame.peer, title, FileDialog.SAVE)
    dlg.setDirectory(lastExportDir.path)
    dlg.setVisible(true)
    for {
      name   <- Option(dlg.getFile)
      parent <- Option(dlg.getDirectory)
    } {
      val f0  = new File(parent, name)
      lastExportDir = f0.parent
      val fmt = f0.ext.toLowerCase match {
        case "jpg" => "jpg"
        case _     => "png"
      }
      val f = f0.replaceExt(fmt)
      import de.sciss.equal.Implicits._

      // only confirm if file name changed because native file dialog already asked
      val ok = f == f0 || !f.exists() || {
        val message = s"<html><body><b>Warning:</b> File already exists:<p>$f<p>Overwrite it?"
        val res = Dialog.showConfirmation(message = message, title = title,
          optionType = Dialog.Options.OkCancel, messageType = Dialog.Message.Warning)
        res === Dialog.Result.Ok
      }
      if (ok) {
        val ggWidth   = new Spinner(mExpWidth)
        val ggHeight  = new Spinner(mExpHeight)
        val ggMargin  = new Spinner(mExpMargin)
        val lbWidth   = new Label("Width:")
        val lbHeight  = new Label("Height:")
        val lbMargin  = new Label("Margin:")
        val lbInfo    = new Label("Specify image dimensions (px)")
        val pMessage = new GroupPanel {
          horizontal = Par(lbInfo, Seq(
            Par(lbWidth, lbHeight, lbMargin), Par(ggWidth, ggHeight, ggMargin)
          ))
          vertical = Seq(lbInfo, Par(lbWidth, ggWidth), Par(lbHeight, ggHeight), Par(lbMargin, ggMargin))
        }
        val res = Dialog.showConfirmation(message = pMessage.peer, title = title, optionType = Dialog.Options.OkCancel)

        if (res === Dialog.Result.Ok) {
          val w         = mExpWidth .getNumber.intValue
          val h         = mExpHeight.getNumber.intValue
          val margin    = mExpMargin.getNumber.intValue
          val imgExport = new BufferedImage(w, h, BufferedImage.TYPE_INT_ARGB)
          val r         = prepareRenderer(imgExport, margin = margin)
          val df        = definition
          val dp        = depthValue
          import ExecutionContext.Implicits.global
          val fut       = Future {
            blocking {
              r.render(df, dp)
              r.g.dispose()
            }
          }
          fut.foreach { _ =>
            try {
              ImageIO.write(imgExport, fmt, f)
            } catch {
              case NonFatal(t) =>
                onEDT {
                  Dialog.showMessage(message = t.getMessage, title = t.getClass.getSimpleName,
                    messageType = Dialog.Message.Error)
                }
            }
          }
        }
      }
    }
  }
}