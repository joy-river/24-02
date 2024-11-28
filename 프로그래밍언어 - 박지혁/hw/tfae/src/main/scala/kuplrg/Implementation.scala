package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match{
      case Num(_) => NumT
      case Add(e1, e2) => {
        val t1 = typeCheck(e1, tenv)
        val t2 = typeCheck(e2, tenv)
        (t1, t2) match {
          case (NumT, NumT) => NumT
          case _ => error("type error")
        }
      }
      case Mul(e1, e2) => {
        val t1 = typeCheck(e1, tenv)
        val t2 = typeCheck(e2, tenv)
        (t1, t2) match {
          case (NumT, NumT) => NumT
          case _ => error("type error")
        }
      }
      case Val(x, e1, e2) => {
        val t1 = typeCheck(e1, tenv)
        typeCheck(e2, tenv + (x -> t1))
      }
      case Id(x) => tenv.get(x) match {
        case Some(t) => t
        case None => error("undefined variable")
      }
      case Fun(x, t, e) => {
        val t1 = typeCheck(e, tenv + (x -> t))
        ArrowT(t, t1)
      }
      case App(e0, e1) => {
        val t0 = typeCheck(e0, tenv)
        val t1 = typeCheck(e1, tenv)
        t0 match {
          case (ArrowT(t3, t4)) => if t3 == t1 then t4 else error("Type error")
          case _ => error("Type error")
        }

      } 
  }
  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
    case Add(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => error("Invalid operation")
      }
    }
    case Mul(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => error("Invalid operation")
      }
    }
    case Val(x, e1, e2) => {
      val v1 = interp(e1, env)
      interp(e2, env + (x -> v1))
    }
    case Fun(x, t, e) => CloV(x, e, env)
    case Id(x) => env.get(x) match {
      case Some(v) => v
      case None => error("Undefined variable")
    }
    case App(e0, e1) => {
      val v0 = interp(e0, env)
      val v1 = interp(e1, env)
      v0 match {
        case CloV(x, e2, env2) => interp(e2, env2 + (x -> v1))
        case _ => error("Not a function")
      }
    }
  }

}
