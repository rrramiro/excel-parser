package parser
import Parser._
import cats.syntax.traverse._
import cats.instances.either._
import cats.instances.list._

object Parser0 {
  def numericRange(workbook: Workbook, name: String): EitherParserError[List[Double]] =
    for {
      area    <- getArea(workbook, name)
      cells   <- area.getAllReferencedCells.toList.traverse(getSafeCell(workbook, _))
      doubles <- cells.traverse(_.asDouble)
    } yield doubles
}
