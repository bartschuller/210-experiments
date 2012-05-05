import com.codecommit.antixml.{Group, Attributes, Elem}

/**
 * Anti-XML literals
 */
object AX {
  def main(args: Array[String]) {
    val e = Elem("foo", Attributes("bar" -> "baz"))
    println(e.toString())
  }
}
