import scala.collection.immutable.PagedSeq
import com.codecommit.antixml._
import java.io.StringReader
import scala.util.parsing.combinator._
import scala.collection.mutable.ListBuffer
import scala.util.parsing.input.PagedSeqReader

class XMLParser extends RegexParsers {
  override def skipWhitespace: Boolean = false

  val placeHolder: Parser[String] = "\u0007"

  lazy val document = opt(declaration) ~> optSpace ~> element <~ optSpace

  lazy val declaration = "<?xml" ~ version ~ (encoding ?) ~ (standalone ?) ~ optSpace ~ "?>"

  lazy val version = space ~ "version" ~ equals ~ string

  lazy val encoding = space ~ "encoding" ~ equals ~ string

  lazy val standalone = space ~ "standalone" ~ equals ~ (yes | no)

  lazy val element = nonEmpty | empty

  lazy val nonEmpty = startTag ~ content ~ endTag >> mkNonEmpty

  lazy val empty = "<" ~> name ~ attributes <~ optSpace <~ "/>" ^^ mkEmpty

  lazy val startTag = "<" ~> name ~ attributes <~ optSpace <~ ">"

  lazy val endTag = "</" ~> name <~ optSpace <~ ">"

  lazy val attributes: Parser[Attributes] = ((space ~> attribute *) ^^ mkAttributes) ~
                   opt(space ~> attributesPlaceHolder) ~
                   ((space ~> attribute *) ^^ mkAttributes) ^^ {
    case first ~ optInterpolated ~ last => Attributes((first ++ optInterpolated.getOrElse(Attributes.empty) ++ last).toSeq:_*)
  }

  lazy val attributesPlaceHolder = Parser { in =>
    placeHolder(in) match {
      case Success(_, in1) => {
        val inWithState = in.asInstanceOf[ReaderWithState[Seq[Any]]]
        val in1WithState = in1.asInstanceOf[ReaderWithState[Seq[Any]]]
        val passState = in1WithState.extraState
        val replacement = inWithState.extraState.head match {
          case a: Attributes => a
          case m: Map[QName, String] => Attributes(m.toSeq:_*)
        }
        Success(replacement, in1WithState.copy(extraState = passState.tail))
      }
      case ns: NoSuccess => ns
    }
  }

  lazy val content: Parser[List[Node]] = (charData ?) ~ ((contentPlaceHolder | element) ~ (charData ?) *) ^^ mkContent

  lazy val contentPlaceHolder = Parser { in =>
    placeHolder(in) match {
      case Success(_, in1) => {
        val inWithState = in.asInstanceOf[ReaderWithState[Seq[Any]]]
        val in1WithState = in1.asInstanceOf[ReaderWithState[Seq[Any]]]
        val passState = in1WithState.extraState
        val replacement = inWithState.extraState.head match {
          case n: Node => n
          case s => Text(s.toString)
        }
        Success(replacement, in1WithState.copy(extraState = passState.tail))
      }
      case ns: NoSuccess => ns
    }
  }

  lazy val attribute = (name <~ equals) ~ (stringPlaceHolder | string)

  lazy val stringPlaceHolder = Parser { in =>
    placeHolder(in) match {
      case Success(_, in1) => {
        val inWithState = in.asInstanceOf[ReaderWithState[Seq[Any]]]
        val in1WithState = in1.asInstanceOf[ReaderWithState[Seq[Any]]]
        val passState = in1WithState.extraState

        Success(inWithState.extraState.head.toString, in1WithState.copy(extraState = passState.tail))
      }
      case ns: NoSuccess => ns
    }
  }

  lazy val equals = optSpace ~ "=" ~ optSpace

  lazy val string = doubleString | singleString

  lazy val optSpace = opt(space)

  val charData = "[^<\u0007]+".r ^^ Text

  val space = """\s+""".r

  val name: Parser[String] = """(:|\w)((\-|\.|\d|:|\w))*""".r

  val doubleString: Parser[String] = "\"" ~> """[^"]*""".r <~ "\""

  val singleString: Parser[String] = "'" ~> "[^']*".r <~ "'"

  val yes: Parser[String] = "\"yes\"" | "'yes'"

  val no: Parser[String] = "\"no\"" | "'no'"

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
    case elName ~ atts => Elem(elName, atts)
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
    val exprs = Seq(Attributes("a"->"data wins", "b" -> "data loses"), "one", Elem("hi", Attributes()))
    val xml = """<foo a="overridden" \u0007 b="literal wins">blie <b attr=\u0007/> bla \u0007</foo>"""
    val reader = ReaderWithState(new PagedSeqReader(PagedSeq.fromReader(new StringReader(xml))), exprs)
    val doc = parseAll(document, reader)
    doc match {
      case Success(result, _) => println(result)
      case Error(msg, _) => println(msg)
      case Failure(msg, _) => println(msg)
    }
  }
} 
