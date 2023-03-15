package exercises03

object Partial {
  def combo[I, T](funcs: List[PartialFunction[I, T]]): I => Option[T] = (f: I) => funcs.find(_.isDefinedAt(f)).map(_(f))
}
