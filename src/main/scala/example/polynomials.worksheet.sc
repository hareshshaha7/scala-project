import scala.annotation.targetName

class Polynomials(nonZeroTerms: Map[Int, Double]):
  def this(bindings: (Int, Double)*) =
    this(bindings.toMap)

  def terms = nonZeroTerms.withDefaultValue(0.0)

  //  @targetName("add")
  //  def +(other: Polynomials): Polynomials =
  //    Polynomials(terms ++ other.terms.map((exp, coeff) => (exp, terms(exp) + coeff)))

  def +(other: Polynomials): Polynomials =
    Polynomials(other.terms.foldLeft(terms)(addTerm))

  def addTerm(terms: Map[Int, Double], term: (Int, Double)): Map[Int, Double] =
    val (exp, coeff) = term
    terms + (exp -> (coeff + terms(exp)))

  override def toString =
    val termString =
      for (exp, coeff) <- terms.toList.sorted.reverse
        yield
          val exponent = if exp == 0 then "" else s"x^$exp"
          if coeff < 0 then s"$coeff$exponent" else s"$coeff$exponent"
    if terms.isEmpty then "0" else termString.mkString(" + ")


//val x = Polynomials(Map(0 -> 5, 1 -> -2, 3 -> 1))
//val y = Polynomials(Map())
//val z = Polynomials(Map(0 -> 3, 2 -> 1, 3 -> 5))
val x = Polynomials(0 -> 5, 1 -> -2, 3 -> 1)
val y = Polynomials()
val z = Polynomials(0 -> 3, 2 -> 1, 3 -> 5)
x + y
y + x
x + z
z + x

