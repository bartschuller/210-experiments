class DynamicTest extends Dynamic {
  def applyDynamic(methodName: String)(args: Any*) {
    println(s"You called '$methodName' method with " +
      s"following arguments: ${args mkString ", "}")
  }
}
