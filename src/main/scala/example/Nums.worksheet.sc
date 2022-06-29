abstract class Nat:
  def isZero: Boolean

  def predecessor: Nat

  def successor: Nat

  def +(that: Nat): Nat

  def -(that: Nat): Nat
end Nat

class Succ(n: Nat) extends Nat :
  def isZero: Boolean = false

  def predecessor: Nat = n

  def successor: Nat = Succ(this)

  def +(that: Nat): Nat = Succ(n + that)

  def -(that: Nat): Nat = if that.isZero then this else n - that.predecessor

  override def toString: String = s"Succ($n)"
end Succ

object Zero extends Nat :
  def isZero: Boolean = true

  def predecessor: Nat = ???

  def successor: Nat = Succ(this)

  def +(that: Nat): Nat = that

  def -(that: Nat): Nat = if that.isZero then this else ???

  override def toString: String = "Zero"
end Zero

val one = Succ(Zero)
val two = Succ(Succ(Zero))
two + one
two - one
one - two
two - two