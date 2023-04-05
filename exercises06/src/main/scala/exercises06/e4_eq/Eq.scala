package exercises06.e4_eq

trait Eq[A] {
  def eqv(a: A, b: A): Boolean
}

object EqInstances {
  implicit val intEq: Eq[Int] = (a: Int, b: Int) => a == b

  implicit val stringEq: Eq[String] = (a: String, b: String) => a == b

  implicit val boolEq: Eq[Boolean] = (a: Boolean, b: Boolean) => a == b

  implicit def optionEq[A](implicit base: Eq[A]): Eq[Option[A]] =
    (a: Option[A], b: Option[A]) =>
      (a, b) match {
        case (None, None)       => true
        case (Some(x), Some(y)) => base.eqv(x, y)
        case _                  => false
      }

  implicit def listEq[A](implicit base: Eq[A]): Eq[List[A]] = (a: List[A], b: List[A]) => a.corresponds(b)(base.eqv)
}

object EqSyntax {
  implicit class EqOps[A](private val a: A) extends AnyVal {
    def eqv(b: A)(implicit ev: Eq[A]): Boolean = ev.eqv(a, b)
    def ===(b: A)(implicit ev: Eq[A]): Boolean = eqv(b)
    def !==(b: A)(implicit ev: Eq[A]): Boolean = !eqv(b)
  }
}

object Examples {
  import EqInstances._
  import EqSyntax._

  1 eqv 1 // возвращает true
  1 === 2 // возвращает false
  1 !== 2 // возвращает true
  // 1 === "some-string" // не компилируется
  // 1 !== Some(2) // не компилируется
  List(true) === List(true) // возвращает true
  List(true) eqv List(true) // возвращает true
}
