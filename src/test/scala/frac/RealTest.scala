package frac

object RealTest extends App {
  import org.parboiled.scala._

  val res1 = TestParser.parseDouble("2.3")
  println(s"RESULT: ${res1.result}")
  val res2 = TestParser.parseDouble("-4.56")
  println(s"RESULT: ${res2.result}")
  val res3 = TestParser.parseDouble("789")
  println(s"RESULT: ${res3.result}")

  object TestParser extends Parser {
    def Decimal = rule {
      Integer ~ optional[Int]("." ~ PosInteger) ~~> { (a: Int, bOpt: Option[Int]) =>
        bOpt.fold(a.toDouble)(b => s"$a.$b".toDouble) // I don't think this is the way to do it...
    }}
    def PosInteger  = rule { Digits ~> (_.toInt) }
    def Integer     = rule { optional[Unit]("-" ~> (_ => ())) ~ PosInteger ~~> { (neg: Option[Unit], num: Int) =>
      if (neg.isDefined) -num else num
    }}
    def Digit       = rule { "0" - "9" }
    def Digits      = rule { oneOrMore(Digit) }

    def parseDouble(input: String): ParsingResult[Double] = ReportingParseRunner(Decimal).run(input)
  }
}