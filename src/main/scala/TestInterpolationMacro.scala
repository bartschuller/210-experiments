import InterpolationMacro.MyInterpolation

object TestInterpolationMacro {
  def main(args: Array[String]) {
    val what = "world"
    val i = mi"Hi there, $what and ${1+1}"
    println(i * 2)
  }
}
