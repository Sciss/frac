// Hanns Holger Rutz in May 2016

package frac

import java.awt.{BasicStroke, Stroke}

/** A stroke operation instructs for a change of stroke. */
trait StrokeOperation extends Symbol {
  def changeStroke(previousStroke: Stroke): Stroke
}

object ConstantStrokeOperation {
  val predefined: Map[String, Stroke] = Map(
    "thin"   -> new BasicStroke(0.5f),
    "medium" -> new BasicStroke(2f  ),
    "thick"  -> new BasicStroke(4f  )
  )

  def apply(width: Double): ConstantStrokeOperation = ConstantStrokeOperation(new BasicStroke(width.toFloat))
  def apply(name: String) : ConstantStrokeOperation = ConstantStrokeOperation(predefined(name))

  def findPredefinedName(c: Stroke): Option[String] = predefined
    .find(_._2 == c)
    .map(_._1)
}
case class ConstantStrokeOperation(stroke: Stroke) extends StrokeOperation {
  require(stroke != null, "stroke must not be null")

  def changeStroke(previousStroke: Stroke): Stroke = stroke

//  override lazy val toString = ConstantStrokeOperation.findPredefinedName(stroke) match {
//    case Some(name) => "{%s}".format(name)
//    case _ => "{%d}".format(stroke.width)
//  }
}

case class IncrementStrokeOperation(factor: Double) extends StrokeOperation {
  println(this)
  def changeStroke(previousStroke: Stroke): Stroke = {
    val w0 = previousStroke match {
      case b: BasicStroke => b.getLineWidth
      // case _ => 1.0
    }
    new BasicStroke((w0 * factor).toFloat)
  }

//  override lazy val toString = "{%+d,%+d,%+d}".format(redIncrement, greenIncrement, blueIncrement)
}