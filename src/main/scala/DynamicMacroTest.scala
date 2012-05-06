object DynamicMacroTest {
  def main(args: Array[String]) {
    val d = new DynamicMacro
    println(d sMethodName)
    println(d iMethodWithLongName)
    println(d anyOtherName)
  }
}
