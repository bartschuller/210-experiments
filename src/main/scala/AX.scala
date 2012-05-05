import collection.immutable.PagedSeq
import com.codecommit.antixml._
import java.io.StringReader
import scala.util.parsing.input.PagedSeqReader

/**
 * Anti-XML literals
 */
object AX {
  implicit class XMLInterpolation(sc: StringContext) {
    object xml {
      def apply(exprs: Any*): Elem = {
        val xml = sc.parts.mkString("\u0007")
        val reader = ReaderWithState(new PagedSeqReader(PagedSeq.fromReader(new StringReader(xml))), exprs)
        val parser = new XMLParser
        val doc = parser.parseAll(parser.document, reader)
        doc match {
          case parser.Success(result, _) => result
          case parser.Error(msg, _) => println(msg); ???
          case parser.Failure(msg, _) => println(msg); ???
        }
      }
    }
  }

  def main(args: Array[String]) {
    val attributes = Attributes("a"->"data wins", "b" -> "data loses")
    val attributeValue = "one"
    val node = Group(Elem("hi", Attributes()), Text(" there"))
    val xml = xml"""<foo a="overridden" $attributes b="literal wins">blie <b attr=$attributeValue/> bla $node</foo>"""
    println(xml)
  }
}
