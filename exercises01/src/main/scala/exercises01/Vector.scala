package exercises01

import scala.math.sqrt

class Vector(val x: Double, val y: Double) {
  def +(other: Vector): Vector = new Vector(other.x + this.x, other.y + this.y)

  def -(other: Vector): Vector = new Vector(this.x - other.x, this.y - other.y)

  def *(scalar: Double): Vector = new Vector(this.x * scalar, this.y * scalar)

  def unary_- : Vector = new Vector(-this.x, -this.y)

  def euclideanLength: Double = sqrt(this.x * this.x + this.y * this.y)

  def normalized: Vector = {
    val length = euclideanLength
    if (length == 0) new Vector(0, 0) else new Vector(this.x / length, this.y / length)
  }

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[Vector]) return false;
    val vector = other.asInstanceOf[Vector]
    this.x == vector.x && this.y == vector.y;
  }

  override def toString: String = "Vector(" + this.x.toString + ", " + this.y.toString + ")"

}

object Vector {
  def fromAngle(angle: Double, length: Double): Vector = new Vector(length * math.cos(angle), length * math.sin(angle))

  def sum(list: List[Vector]): Vector = list.foldRight(new Vector(0, 0))((A, B) => new Vector(A.x + B.x, A.y + B.y))

  def unapply(arg: Vector): Option[(Double, Double)] = Option(arg.x, arg.y)
}
