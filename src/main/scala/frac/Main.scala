/*
 * Copyright (C) 2011 Julien Letrouit
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

import swing._
import event._
import Swing._
import BorderPanel.Position._
import java.awt.{GraphicsEnvironment, Font, Color}

object Main extends SimpleSwingApplication {

    //GraphicsEnvironment.getLocalGraphicsEnvironment.getAvailableFontFamilyNames.foreach(println(_))

    val SEGMENT_STAT_TEMPLATE = "Turtle moves: %d"
    val TOKEN_STAT_TEMPLATE = "Sequence length: %d"
    val TIME_STAT_TEMPLATE = "Drawing duration: %d ms"
    val CODE_COLOR = 80
    val parser = new RuleBasedParser
    val definitions = new DefaultDefinitionRepository().getDefinitions
    var definition = parser.parse(definitions(0).source)

    val segmentStat = new Label("", null, Alignment.Left)
    val tokensStat = new Label("", null, Alignment.Left)
    val timeStat = new Label("", null, Alignment.Left)
    val editorLabel = new Label("Editor:", null, Alignment.Left) {
        border = EmptyBorder(5, 2, 5, 2)
        font = font.deriveFont(Font.BOLD)
    }
    val generateBtn = new Button("Refresh")
    val minusBtn = new Button("-")
    val plusBtn = new Button("+")
    val menu = new MenuBar {
        contents += new Menu("Load example") {
            definitions.foreach(ds => contents += new MenuItem(Action(ds.name) {
                selectExample(ds)
            }))
        }
        contents += new Menu("Help") {
            contents += new MenuItem("License")
            contents += new MenuItem("User manual")
        }
    }
    val editor = new TextArea(definitions.head.source, 5, 20) {
        font = new Font("Monospaced", Font.BOLD, 16)
        foreground = new Color(CODE_COLOR, CODE_COLOR, CODE_COLOR)
    }
    val depth = new TextField("1", 3) {
        verifier = (txt: String) => try { txt.toInt ; true} catch { case t: Throwable => false }
    }
    val fractalPanel = new Panel {
        override def paintComponent(g: Graphics2D) {
            super.paintComponent(g)
            g.setColor(new Color(100,100,100))
            val stats = new GraphicsRenderer(g).render(definition, depth.text.toInt)
            segmentStat.text = SEGMENT_STAT_TEMPLATE.format(stats.segments)
            tokensStat.text = TOKEN_STAT_TEMPLATE.format(stats.tokens)
            timeStat.text = TIME_STAT_TEMPLATE.format(stats.time)
        }
    }

    val definitionPanel = new BorderPanel {
        val rightSection = new BoxPanel(Orientation.Vertical) {
            contents += editor
            contents += segmentStat
            contents += tokensStat
            contents += timeStat
        }
        val bottomBar = new FlowPanel {
            contents += minusBtn
            contents += depth
            contents += plusBtn
            contents += generateBtn
        }
        layout(editorLabel) = North
        layout(rightSection) = Center
        layout(bottomBar) = South
    }

    val center = new SplitPane(Orientation.Vertical, fractalPanel, definitionPanel) {
        continuousLayout = true
        oneTouchExpandable = true
        dividerLocation = 1200
    }

    lazy val topFrame = new MainFrame {
        title = "Frac v1.0"
        contents = new BorderPanel {
            preferredSize = (1600,1000)
            opaque = true
            layout(center) = Center
        }
        menuBar = menu
        centerOnScreen()
        listenTo(this, minusBtn, plusBtn, generateBtn)
        defaultButton = generateBtn
        reactions += {
            case ButtonClicked(`minusBtn`) =>
                depth.text = (depth.text.toInt - 1).toString
                refresh()
            case ButtonClicked(`plusBtn`) =>
                depth.text = (depth.text.toInt + 1).toString
                refresh()
            case ButtonClicked(`generateBtn`) =>
                refresh()
            case WindowClosing(e) =>
                println("Exiting...")
                System.exit(0)
        }
    }

    def top = topFrame

    def selectExample(example: DefinitionSource)
    {
        editor.text = example.source
        depth.text = "1"
        refresh()
    }

    def refresh()
    {
        try {
            definition = parser.parse(editor.text)
            fractalPanel.repaint()
        }
        catch {
            case t: Throwable =>
                Dialog.showMessage(message = t.getMessage, title = "Syntax error", messageType = Dialog.Message.Error)
        }
    }

}