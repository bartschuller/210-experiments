import java.util.regex.Pattern

object StringInterpolation {
  def main(args: Array[String]) {
    stringInterpolation()
  }

  implicit class MyInterpolation(s: StringContext) {
    object my {
      def apply(exprs: Any*) = {
        s.s(exprs:_*)
      }
      def unapplySeq(target: String): Option[Seq[String]] = {
        val re = s.parts.map(Pattern.quote(_)).mkString("(.*?)").r
        re.unapplySeq(target)
      }
    }
  }

  def stringInterpolation() {
    val age = 43.0
    val name = "Bart"
    val s1 = f"$name is $age%2.0f ${"ye"+"ars"} old"
    println(s1)
    val my"Bart is ${extracted} $units old" = s1
    println(s"$extracted $units")
  }
}
