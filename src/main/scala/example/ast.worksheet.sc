enum Expr:
  case Num(n: Int)
  case Sum(n1: Expr, n2: Expr)
  case Var(s: String)
  case Prod(n1: Expr, n2: Expr)

def eval(e: Expr): Int = e match {
  case Expr.Num(n) => n
  case Expr.Sum(n1, n2) => eval(n1) + eval(n2)
  case Expr.Prod(m1, m2) => eval(m1) * eval(m2)
}

def show(e: Expr): String = e match
  case Expr.Num(n) => n.toString
  case Expr.Sum(n1, n2) => s"${show(n1)} + ${show(n2)}"
  case Expr.Var(s) => s
  case Expr.Prod(m1, m2) => s"${showP(m1)} * ${showP(m2)}"

def showP(e: Expr): String = e match
  case e: Expr.Sum => s"(${show(e)})"
  case _ => show(e)

import Expr.{Sum, Num}
eval(Num(1))
val expr = Sum(Num(1), Num(3))
eval(expr)

show(Num(1))
show(expr)

import Expr.{Prod, Var}
val expr2 = Prod(Num(2), Num(3))
show(expr2)

val expr3 = Prod(expr, Num(3))
show(expr3)

show(Sum(Prod(Num(2), Var("X")), Var("Y"))) // 2 * X + Y
show(Prod(Sum(Num(2), Var("X")), Var("Y"))) // (2 + X) * Y

enum Directions(val x: Int, val y:Int):
  case Left extends Directions(1, 0)
  case Right extends Directions(0, 1)
  case Up extends Directions(-1, 0)
  case Down extends Directions(0, -1)

  def turnLeft = Directions.values((ordinal+1) % 4)
end Directions

val r = Directions.Right
val u = r.turnLeft
val v= (u.x, u.y)

