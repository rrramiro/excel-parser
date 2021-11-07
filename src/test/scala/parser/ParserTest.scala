package parser

import cats.data.Writer
import cats.syntax.all._
import cats.instances.list._
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.should.Matchers
import Parser._

class ParserTest extends AnyFreeSpec with Matchers {

  val example: Workbook = load("example.xlsx")

  "Reader" - {
    "numeric" in {
      numeric("ExplorationFee").parse(example) shouldEqual 1.4.asRight
    }
    "numeric range" in {
      numericRange("OilProd").parse(example) shouldEqual
        List(10.12, 12.34, 8.83, 6.23, 9.18, 12.36, 16.28, 18.25, 20.01).asRight
    }
    "numeric range when the range is not numeric" in {
      numericRange("PrimaryProduct").parse(example) shouldEqual
        ParserError
          .invalidFormat("Sheet1!B4", "double", "Cannot get a NUMERIC value from a STRING cell")
          .asLeft
    }
    "nonexistent range" in {
      numericRange("foo").parse(example) shouldEqual ParserError.missingName("foo").asLeft
    }
    "string" in {
      string("PrimaryProduct").parse(example) shouldEqual "Oil".asRight
    }
    "Semigroupal" in {
      (numeric("ExplorationFee"), numeric("PostExplorationFee")).tupled
        .parse(example) shouldEqual (1.4, 5.8).asRight
    }
    "Semigroupal 2" in {
      (numericRange("OilProd"), numericRange("GasProd"))
        .mapN(Production)
        .parse(example) shouldEqual
        Production(
          List(10.12, 12.34, 8.83, 6.23, 9.18, 12.36, 16.28, 18.25, 20.01),
          List(0.0, 0.0, 0.0, 3.2, 6.5, 6.38, 5.72, 4.54, 6.99)
        ).asRight
    }
    "Coproduct" in {
      val technicalFee = numeric("TechnicalFee").map(TechnicalFee)
      val explorationFee =
        (numeric("ExplorationFee"), numeric("PostExplorationFee")).mapN(ExplorationFee)
      val fee = technicalFee.widen[Fee] <+> explorationFee.widen[Fee]
      fee.parse(example) shouldEqual ExplorationFee(1.4, 5.8).asRight
    }
  }

  "Logger" - {
    "numeric" in {
      {
        for {
          explorationFee <- numeric("ExplorationFee").tryParse(example)
          _              <- numericRange("foo").tryParse(example)
        } yield explorationFee
      }.run shouldEqual (
        List(ParserError.missingName("foo")) -> (Some(1.4: Double): Option[Double])
      )
    }
  }

  implicit class ParserWrapper[A](parser: Parser[A]) {
    def tryParse(workbook: Workbook): Writer[List[ParserError], Option[A]] =
      parser.parse(workbook) match {
        case Left(err) => Writer(List(err), None)
        case Right(v)  => Writer(List.empty, Some(v))
      }
  }
}
