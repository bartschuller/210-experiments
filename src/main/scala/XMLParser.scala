import com.codecommit.antixml._
import java.io.{StringReader, FileReader}
import scala.util.parsing.combinator._
import scala.collection.mutable.ListBuffer

class XMLParser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  def placeHolder = "\u0007"

  def document = opt(declaration) ~> optSpace ~> element <~ optSpace

  def declaration = "<?xml" ~ version ~ (encoding ?) ~ (standalone ?) ~ optSpace ~ "?>"

  def version = space ~ "version" ~ equals ~ string

  def encoding = space ~ "encoding" ~ equals ~ string

  def standalone = space ~ "standalone" ~ equals ~ (yes | no)

  def element = nonEmpty | empty

  def nonEmpty = startTag ~ content ~ endTag >> mkNonEmpty

  def empty = "<" ~> name ~ attributes <~ optSpace <~ "/>" ^^ mkEmpty

  def startTag = "<" ~> name ~ attributes <~ optSpace <~ ">"

  def endTag = "</" ~> name <~ optSpace <~ ">"

  def attributes = (space ~> attribute *) ^^ mkAttributes

  def content: Parser[List[Node]] = (charData ?) ~ (element ~ (charData ?) *) ^^ mkContent

  def attribute = (name <~ equals) ~ (placeHolder | string)

  def equals = optSpace ~ "=" ~ optSpace

  def string = doubleString | singleString

  def optSpace = space ?

  def charData = "[^<]+".r ^^ Text

  def space = """\s+""".r

  def name = """(:|\w)((\-|\.|\d|:|\w))*""".r

  def doubleString = "\"" ~> """[^"]*""".r <~ "\""

  def singleString = "'" ~> "[^']*".r <~ "'"

  def yes = "\"yes\"" | "'yes'"

  def no = "\"no\"" | "'no'"

  private def mkAttributes = (list: List[String ~ String]) =>
    Attributes(list.map{case a~b => (QName(None, a), b)}:_*)

  private type NonEmpty = String ~ Attributes ~ List[Node] ~ String

  private def mkNonEmpty: NonEmpty => Parser[Node] = {
    case startName ~ atts ~ children ~ endName =>
      if (startName == endName)
        success(Elem(None, startName, atts, Map(), Group(children: _*)))
      else
        err("tag mismatch")
  }

  private def mkEmpty: String ~ Attributes => Node = {
    case name ~ atts => Elem(name, atts)
  }

  private type Content = Option[Text] ~ List[Node ~ Option[Text]]

  private def mkContent: Content => List[Node] = {
    case Some(txt) ~ rest => txt :: unpackContent(rest)
    case None ~ rest => unpackContent(rest)
  }

  private def unpackContent(contents: List[Node ~ Option[Text]]): List[Node] = {
    val acc = new ListBuffer[Node]
    for (node ~ chars <- contents) {
      acc += node
      if (chars.isDefined) acc += chars.get
    }
    acc.toList
  }
}

object CombinatorDemo extends XMLParser {
  def main(args: Array[String]) {
    val exprs = Seq("one", "two")
    val reader = new StringReader("""<foo a="b">blie <b attr=\u0007/> bla \u0007</foo>""")
    val doc = parseAll(document, reader)
    doc match {
      case Success(result, _) => println(result)
      case Error(msg, _) => println(msg)
      case Failure(msg, _) => println(msg)
    }
  }
} 
