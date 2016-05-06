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

import java.awt.Color
import java.util.Date

import scala.math._
import scala.swing.Graphics2D

case class Point(x: Double, y: Double)
case class RendererStats(turtleMoves: Int, turtleTurns: Int, sequenceLength: Int, duration: Long)

/** Renders the given definition on an AWT graphics */
class GraphicsRenderer(val g: Graphics2D, val cWidth: Int, val cHeight: Int) extends RunState {
  var isRunning = true

  private[this] val MARGIN                = 20
  private[this] var position              = Point(0, 0)
  private[this] var heading               = 0.0
  private[this] var turnAngle             = Pi / 2
  private[this] var (minPoint, maxPoint)  = (Point(0, 0), Point(0, 0))
  private[this] var travelLength          = 10.0
  private[this] var stateStack            = List.empty[TurtleState]
  private[this] var (turtleMovesCounter, turtleTurnsCounter, sequenceCounter) = (0, 0, 0)
  private[this] var strokeColor           = Color.black

  private case class TurtleState(position: Point, heading: Double, moveLength: Double, strokeColor: Color)

  def render(definition: FracDef, depth: Int): RendererStats = {
    val start = new Date().getTime
    // Dry run to compute size
    init(Point(0, 0) -> 10.0, definition)
    definition.execute(this, depth, callback(draw = false, definition.scaleRatio))

    // Center, scale, and draw
    init(computeTransformation, definition)
    definition.execute(this, depth, callback(draw = true, definition.scaleRatio))

    RendererStats(turtleMovesCounter, turtleTurnsCounter, sequenceCounter, new Date().getTime - start)
  }

  private def init(transformation: (Point, Double), definition: FracDef): Unit = {
    position            = transformation._1
    heading             = definition.startingPoint match {
      case StartingPoint.Left   => 0.0
      case StartingPoint.Bottom => -Pi / 2
    }
    travelLength        = transformation._2
    turnAngle           = definition.turnAngle
    minPoint            = Point(0, 0)
    maxPoint            = Point(0, 0)
    stateStack          = List.empty[TurtleState]
    turtleMovesCounter  = 0
    turtleTurnsCounter  = 0
    sequenceCounter     = 0
  }

  /** Look what is the scaling and translation that must be applied to fill the drawing space */
  private def computeTransformation: (Point, Double) = {
    val (boundsWidth, boundsHeight) = (cWidth.toDouble - 2 * MARGIN, cHeight.toDouble - 2 * MARGIN)
    val (width, height) = (maxPoint.x - minPoint.x, maxPoint.y - minPoint.y)
    val (boundsRatio, ratio) = (boundsWidth / boundsHeight, width / height)

    val scale = if (ratio > boundsRatio) boundsWidth / width else boundsHeight / height
    val (scaledWidth, scaledHeight) = (width * scale, height * scale)

    (Point(MARGIN - minPoint.x * scale + (boundsWidth - scaledWidth) / 2, MARGIN - minPoint.y * scale + (boundsHeight - scaledHeight) / 2), scale * 10.0)
  }


  /** Interpret the given character and update state accordingly */
  private def callback(draw: Boolean, scaleRatio: Double)(c: Symbol): Unit = {
    c match {
      case RuleReference('+', repetitionCount) =>
        heading -= turnAngle * repetitionCount
        turtleTurnsCounter += repetitionCount
      case RuleReference('-', repetitionCount) =>
        heading += turnAngle * repetitionCount
        turtleTurnsCounter += repetitionCount
      case RuleReference('F', repetitionCount) =>
        move(draw, repetitionCount)
        turtleMovesCounter += repetitionCount
      case RuleReference('f', repetitionCount) =>
        move(draw = false, repetitionCount)
        turtleMovesCounter += repetitionCount
      case RuleReference('[', _) =>
        stateStack ::= TurtleState(position, heading, travelLength, strokeColor)
      case RuleReference(']', _) =>
        val (state :: newStack) = stateStack
        heading       = state.heading
        travelLength  = state.moveLength
        position      = state.position
        strokeColor   = state.strokeColor
        stateStack    = newStack
      case RuleReference('>', _) =>
        travelLength *= scaleRatio
      case RuleReference('<', _) =>
        travelLength /= scaleRatio
      case colorStatement : ColorOperation =>
        strokeColor = colorStatement.changeColor(strokeColor)
        g.setColor(strokeColor)
      case _ =>
        () // Ignores all other characters
    }

    sequenceCounter += 1
  }

  /** Move the turtle, and optionally draws the movement */
  private def move(draw: Boolean, repetitionCount: Int): Unit = {
    val newPoint = Point(
        position.x + travelLength * repetitionCount * cos(heading),
        position.y + travelLength * repetitionCount * sin(heading))

    if (newPoint.x < minPoint.x) minPoint = minPoint.copy(x = newPoint.x)
    if (newPoint.y < minPoint.y) minPoint = minPoint.copy(y = newPoint.y)
    if (newPoint.x > maxPoint.x) maxPoint = maxPoint.copy(x = newPoint.x)
    if (newPoint.y > maxPoint.y) maxPoint = maxPoint.copy(y = newPoint.y)

    if (draw) g.drawLine(position.x.toInt, position.y.toInt, newPoint.x.toInt, newPoint.y.toInt)

    position = newPoint
  }
}