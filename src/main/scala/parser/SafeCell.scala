package parser

import java.util.Date

import cats.syntax.either._

import scala.reflect.ClassTag

case class SafeCell(cell: Cell) {
  val reference = s"${cell.getSheet.getSheetName}!${cell.getAddress}"

  def asDouble: EitherParserError[Double] = catchNonFatal(_.getNumericCellValue)

  def asString: EitherParserError[String] =
    catchNonFatal(_.getStringCellValue) //getRichStringCellValue

  def asBoolean: EitherParserError[Boolean] = catchNonFatal(_.getBooleanCellValue)

  def asDate: EitherParserError[Date] = catchNonFatal(_.getDateCellValue)

  def asError: EitherParserError[Byte] = catchNonFatal(_.getErrorCellValue)

  private def catchNonFatal[T](
      f: Cell => T
  )(implicit classTag: ClassTag[T]): EitherParserError[T] =
    Either
      .catchNonFatal(f(cell))
      .leftMap(e =>
        ParserError.invalidFormat(reference, classTag.runtimeClass.getName, e.getMessage)
      )

  def asInt: EitherParserError[Int] =
    for {
      d <- asDouble
      r <- Either.cond(
             d.isValidInt,
             d.toInt,
             ParserError.invalidFormat(reference, "Int", s"$d is not an Int")
           )
    } yield r

}
