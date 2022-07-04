trait Expr

case class Num(n: Int) extends Expr

case class Sum(n1: Expr, n2: Expr) extends Expr

case class Var(s: String) extends Expr

case class Prod(n1: Expr, n2: Expr) extends Expr

def eval(e: Expr): Int = e match {
  case Num(n) => n
  case Sum(n1, n2) => eval(n1) + eval(n2)
  case Prod(m1, m2) => eval(m1) * eval(m2)
}

def show(e: Expr): String = e match
  case Num(n) => n.toString
  case Sum(n1, n2) => s"${show(n1)} + ${show(n2)}"
  case variable(s) => s
  case Prod(m1, m2) => s"${showP(m1)} * ${showP(m2)}"

def showP(e: Expr): String = e match
  case e: Sum => s"(${show(e)})"
  case _ => show(e)

eval(Num(1))
val expr = Sum(Num(1), Num(3))
eval(expr)

show(Num(1))
show(expr)

val expr2 = Prod(Num(2), Num(3))
show(expr2)

val expr3 = Prod(expr, Num(3))
show(expr3)

show(Sum(Prod(Num(2), Var("X")), Var("Y"))) // 2 * X + Y
show(Prod(Sum(Num(2), Var("X")), Var("Y"))) // (2 + X) * Y