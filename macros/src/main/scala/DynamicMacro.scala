import language.experimental.macros
import reflect.makro.Context

class DynamicMacro extends Dynamic {
  def selectDynamic(field: String): Any = macro DynamicMacro.selectDynamic_impl
}

object DynamicMacro {
  def selectDynamic_impl(c: Context)(field: c.Expr[String]): c.Expr[Any] = {
    import c.mirror._
    field.tree match {
      case Literal(Constant(method: String)) => method.head match {
        case 's' => Expr[String](Literal(Constant(method.tail)))
        case 'i' => Expr[Int](Literal(Constant(method.length)))
        case _ => field
      }
      case _ => sys.error("selectDynamic without a literal string? Weird")
    }
  }
}
