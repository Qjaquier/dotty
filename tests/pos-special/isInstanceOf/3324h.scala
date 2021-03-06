object Test {
  trait Marker
  def foo[T](x: T) = x match {
    case _: (T & Marker)       => // no warning
    case _: T with String      => // scalac emits a warning
    case _ =>
  }
}