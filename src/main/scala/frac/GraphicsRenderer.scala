package frac

import java.awt.Graphics
import collection.immutable.Stack
import java.util.Date

case class Point(x: Double, y: Double)
case class RendererStats(turtleMoves: Int, turtleTurns: Int, sequenceLength: Int, duration: Long)

class GraphicsRenderer(g: Graphics) extends Renderer[RendererStats]
{
    private val MARGIN = 20
    private var position = Point(0, 0)
    private var heading = 0.0
    private var turnAngle = math.Pi / 2
    private var (minPoint, maxPoint) = (Point(0, 0), Point(0, 0))
    private var travelLength = 10.0
    private var stateStack = Stack.empty[TurtleState]
    private var (turtleMovesCounter, turtleTurnsCounter, sequenceCounter) = (0, 0, 0)

    private case class TurtleState(position: Point, heading: Double, moveLength: Double)

    def render(definition: Definition, depth: Int) : RendererStats =
    {
        val start = new Date().getTime
        // Dry run to compute size
        init(Point(0, 0) -> 10.0, definition)
        definition.run(depth, callback(false, definition.scaleRatio))

        // Center, scale, and draw
        init(computeTransformation, definition)
        definition.run(depth, callback(true, definition.scaleRatio))

        RendererStats(turtleMovesCounter, turtleTurnsCounter, sequenceCounter, new Date().getTime - start)
    }

    private def init(transformation: (Point, Double), definition: Definition)
    {
        position = transformation._1
        heading = definition.startingPoint match {
            case StartingPoint.Left => 0.0
            case StartingPoint.Bottom => -math.Pi / 2
        }
        travelLength = transformation._2
        this.turnAngle = definition.turnAngle
        minPoint = Point(0, 0)
        maxPoint = Point(0, 0)
        stateStack = Stack.empty[TurtleState]
        turtleMovesCounter = 0
        turtleTurnsCounter = 0
        sequenceCounter = 0
    }

    private def computeTransformation =
    {
        val (boundsWidth, boundsHeight) = (g.getClipBounds.width.toDouble - 2 * MARGIN, g.getClipBounds.height.toDouble - 2 * MARGIN)
        val (width, height) = (maxPoint.x - minPoint.x, maxPoint.y - minPoint.y)
        val (boundsRatio, ratio) = (boundsWidth / boundsHeight, width / height)

        val scale = if (ratio > boundsRatio) boundsWidth / width else boundsHeight / height
        val (scaledWidth, scaledHeight) = (width * scale, height * scale)

        (Point(MARGIN - minPoint.x * scale + (boundsWidth - scaledWidth) / 2, MARGIN - minPoint.y * scale + (boundsHeight - scaledHeight) / 2), scale * 10.0)
    }

    private def callback(draw: Boolean, scaleRatio: Double)(c: Char)
    {
        c match {
            case '+' =>
                heading -= turnAngle
                turtleTurnsCounter += 1
            case '-' =>
                heading += turnAngle
                turtleTurnsCounter += 1
            case 'F' =>
                move(draw)
                turtleMovesCounter += 1
            case 'f' =>
                move(false)
                turtleMovesCounter += 1
            case '['=>
                stateStack = stateStack.push(TurtleState(position, heading, travelLength))
            case ']'=>
                val (state, newStack) = stateStack.pop2
                heading = state.heading
                travelLength = state.moveLength
                position = state.position
                stateStack = newStack
            case '>' => travelLength *= scaleRatio
            case '<' => travelLength /= scaleRatio
            case _ => () // Ignores all other characters
        }

        sequenceCounter += 1
    }

    private def move(draw: Boolean)
    {
        val newPoint = Point(
            position.x + travelLength * math.cos(heading),
            position.y + travelLength * math.sin(heading))

        if (newPoint.x < minPoint.x) minPoint = minPoint.copy(x = newPoint.x)
        if (newPoint.y < minPoint.y) minPoint = minPoint.copy(y = newPoint.y)
        if (newPoint.x > maxPoint.x) maxPoint = maxPoint.copy(x = newPoint.x)
        if (newPoint.y > maxPoint.y) maxPoint = maxPoint.copy(y = newPoint.y)

        if (draw) g.drawLine(position.x.toInt, position.y.toInt, newPoint.x.toInt, newPoint.y.toInt)

        position = newPoint
    }
}