import language.experimental.macros
import scala.reflect.makro.Context

object InterpolationMacro {
  implicit class MyInterpolation(s: StringContext) {
    object mi {
      def apply(interpolated: Any*): Any = macro mi_impl
    }
  }
  def mi_impl(c: Context)(interpolated: c.Expr[Any]*): c.Expr[Any] = {
    import c.mirror._
    val result = c.prefix.tree match {
      case Select(Apply(_, List(Apply(_, parts))), _) =>
        val stringList = for(Literal(Constant(stringConstant: String)) <- parts) yield stringConstant
        println(s"reconstituted string: ${stringList.mkString("<>")}")
      case x =>
        sys.error("Unexpected tree: " + showRaw(x))
    }
    interpolated foreach { arg =>
      println("argument: "+showRaw(arg.tree) + "type: "+arg.tree.tpe)
    }
    c.reify("return value")
  }
}
