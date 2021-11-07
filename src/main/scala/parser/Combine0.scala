package parser
import Parser._
import cats.syntax.all._

object Combine0 {
  val technicalFee: Parser[TechnicalFee] = numeric("TechnicalFee").map(TechnicalFee)

  val explorationFee: Parser[ExplorationFee] =
    (numeric("ExplorationFee"), numeric("PostExplorationFee")).mapN(ExplorationFee)

  def combine[A](p1: Parser[A], p2: Parser[A]): Parser[A] = new Parser[A] {
    def parse(workbook: Workbook): Either[ParserError, A] =
      p1.parse(workbook) orElse p2.parse(workbook)
  }
}
