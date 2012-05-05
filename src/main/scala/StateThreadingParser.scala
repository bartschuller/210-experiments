import collection.immutable.PagedSeq
import java.io.StringReader
import scala.util.parsing.combinator.RegexParsers
import util.parsing.input.PagedSeqReader

trait Person
case object Male extends Person
case class Female(gift: Option[String]) extends Person

class StateThreadingParser extends RegexParsers {
  lazy val seating = rep(group1 | group2)
  lazy val group1 = f~m~f
  lazy val group2 = f~m

  lazy val m: Parser[Person] = "M" ^^ { case _ => Male }
  lazy val f = gifted(rawF)
  lazy val rawF: Parser[Female] = "F" ^^ { case _ => Female(None) }

  lazy val gifted = (p: Parser[Female]) => Parser { in =>
    p(in) match {
      case Success(female, in1) => {
        val inWithState = in.asInstanceOf[ReaderWithState[List[String]]]
        val in1WithState = in1.asInstanceOf[ReaderWithState[List[String]]]
        val passState = in1WithState.extraState

        println("handing out "+inWithState.extraState.headOption)
        Success(female.copy(inWithState.extraState.headOption), in1WithState.copy(extraState = if (passState.isEmpty) passState else passState.tail))
      }
      case ns: NoSuccess => ns
    }
  }
}

object TestStateThreading extends StateThreadingParser {
  def main(args: Array[String]) {
    val gifts = List("one", "two", "three")
    val reader = new StringReader("FMFFM")
    val readerWithState = ReaderWithState(new PagedSeqReader(PagedSeq.fromReader(reader)), gifts)
    val doc = parseAll(seating, readerWithState)
    doc match {
      case Success(result, _) => println(result)
      case Error(msg, _) => println(msg)
      case Failure(msg, _) => println(msg)
    }
  }
}
