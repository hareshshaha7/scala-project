class Rational(x: Int, y: Int):
  require(y > 0, "Denominator must be positive!!")

  def this(x: Int) = this(x, 1)

  private def gcd(a: Int, b: Int): Int =
    if b == 0 then a else gcd(b, a % b)

  def numerator = x / gcd(x.abs, y)

  def denominator = y / gcd(x.abs, y)

  def add(that: Rational): Rational =
    new Rational((this.numerator * that.denominator) + (this.denominator * that.numerator), this.numerator * that.denominator)

  def subtract(that: Rational): Rational =
    new Rational((this.numerator * that.denominator) - (this.denominator * that.numerator), this.numerator * that.denominator)

  def multiply(that: Rational): Rational =
    new Rational(this.numerator * that.numerator, this.denominator * that.denominator)

  def divide(that: Rational): Rational =
    new Rational(this.numerator * that.denominator, this.denominator * that.numerator)

  def negate: Rational = Rational(-this.numerator, denominator)

  private def less(that: Rational): Boolean =
    this.numerator * that.denominator < this.denominator * that.numerator

  def min(that: Rational): Rational =
    if this.less(that) then this else that

  def max(that: Rational): Rational =
    if this.less(that) then that else this

  override def toString = s"${numerator / gcd(x.abs, y)}/${denominator / gcd(x.abs, y)}"
end Rational

val r = new Rational(5, 0)
val rNew = new Rational(5)
println(rNew)

val r1 = new Rational(1, 2)
println(r1)

val r2 = new Rational(4, 8)
val r3 = new Rational(3, 4)
println(r1.add(r2))
println(r1.subtract(r2))
println(r1.multiply(r2))
println(r1.divide(r2))

println(r1.add(r2).subtract(r3))
println(r1.negate)

println(r1.min(r2))
println(r2.max(r3))

extension (x: Rational) {
  def +(y: Rational): Rational = x.add(y)
  def -(y: Rational): Rational = x.subtract(y)
  def *(y: Rational): Rational = x.multiply(y)
  def /(y: Rational): Rational = x.divide(y)

  // Use infix to declare alphanumeric method with single parameter as operator
  infix def min(y: Rational): Rational = x.min(y)
  infix def max(y: Rational): Rational = x.max(y)
}

println(r1 + r3) // is shorthand for r1.+(r3)
println(r1 - r3) // is shorthand for r1.-(r3)
println(r1 * r3) // is shorthand for r1.*(r3)
println(r1 / r3) // is shorthand for r1./(r3)

println(r1 min r3) // is shorthand for r1.min(r3)
println(r1 max r3) // is shorthand for r1.max(r3)
