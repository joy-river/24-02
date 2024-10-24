package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = {
    expr match {
      case Num(n) => NumV(n)
      case Add(l, r) => {
        lazy val v1 = strict(interp(l, env))
        lazy val v2 = strict(interp(r, env))



        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
          case _ => error("invalid operation")
        }
      }
      case Mul(l, r) => {
        lazy val v1 = strict(interp(l, env))
        lazy val v2 = strict(interp(r, env))

        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
          case _ => error("invalid operation")
        }
      }

      case Id(id) => {
        env.get(id) match {
          case Some(v) => v
          case _ => error("free identifier")
        }
      }

      case Fun(p, b) => CloV(p, b, env)

      case App(f, a) => {
        lazy val v1 = strict(interp(f, env))
        
        v1 match {
          case CloV(p, b, e) => {
            interp(b, e + (p -> ExprV(a, env)))
          }
          case _ => error("not a function")
        }

      }
    }
  }

  def strict(v: Value): Value = {
    v match {
      case ExprV(e, env) => strict(interp(e, env))
      case _ => v
    }
  }

}
