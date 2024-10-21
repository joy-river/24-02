package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
    case Bool(b) => BoolV(b)
    case Id(id) => {
      env.get(id) match {
        case Some(v) => v
        case _ => error("free identifier")
      }
    }
    case Add(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => error("invalid operation")
      }
    }
    case Mul(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => error("invalid operation")
      }
    }
    case Div(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => if (n2 == 0) error("invalid operation") else NumV(n1 / n2)
        case _ => error("invalid operation")
      }
    }
    case Mod(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => if (n2 == 0) error("invalid operation") else NumV(n1 % n2)
        case _ => error("invalid operation")
      }
    }
    case Eq(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
        case _ => error("invalid operation")
      }
    }

    case Lt(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
        case _ => error("invalid operation")
      }
    }

    case Fun(p, b) => CloV(p, b, () => env)

    case Rec(n, p, b, s) => {
      lazy val newEnv : Env = env + (n -> CloV(p, b, () => newEnv))
      interp(s, newEnv)
    }
    
    case App(f, e) => {
      val v1 = interp(f, env)
      val v2 = interp(e, env)

      v1 match {
        case CloV(p, b, e) => interp(b, e() + (p -> v2))
        case _ => error("not a function")
      }
    }

    case If(c, te, ee) => {
      val v1 = interp(c, env)

      v1 match {
        case BoolV(b) => if b then interp(te, env) else interp(ee, env)
        case _ => error("not a boolean")
      }
    }

  }
}
