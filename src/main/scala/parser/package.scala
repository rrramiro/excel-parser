import org.apache.poi.ss.usermodel.WorkbookFactory

package object parser {
  type EitherParserError[T] = Either[ParserError, T]
  type Workbook             = org.apache.poi.ss.usermodel.Workbook
  type WorkbookFactory      = org.apache.poi.ss.usermodel.WorkbookFactory
  type AreaReference        = org.apache.poi.ss.util.AreaReference
  type CellReference        = org.apache.poi.ss.util.CellReference
  type Cell                 = org.apache.poi.ss.usermodel.Cell
  def load(fileName: String): Workbook =
    WorkbookFactory.create(
      Thread.currentThread().getContextClassLoader.getResourceAsStream(fileName)
    )
  def product[A, B](pa: Parser[A], pb: Parser[B]): Parser[(A, B)] =
    new Parser[(A, B)] {
      def parse(workbook: Workbook): EitherParserError[(A, B)] =
        for {
          a <- pa.parse(workbook)
          b <- pb.parse(workbook)
        } yield (a, b)
    }
  lazy val workbook = load("example.xlsx")
}
