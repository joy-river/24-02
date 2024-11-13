package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def interpCPS(expr: Expr, env: Env, k: Value => Value): Value = expr match {
    case Num(n) => k(NumV(n))
    case Add(l, r) => interpCPS(l, env, {
      lv => interpCPS(r, env, {
        rv => k(
          (lv, rv) match {
            case (NumV(ln), NumV(rn)) => NumV(ln + rn)
            case _ => error("Invalid operation")
          }
        )
      })
    })

    case Mul(l, r) => interpCPS(l, env, {
      lv => interpCPS(r, env, {
        rv => k(
          (lv, rv) match {
            case (NumV(ln), NumV(rn)) => NumV(ln * rn)
            case _ => error("invalid operation")
          }
        )
      })
    })

    case Id(id) => {
      k(env.get(id) match {
        case Some(v) => v
        case None => error("free identifier")
      })
    }

    case Fun(p, b) => {
      k(CloV(p, b, env))
    }

    case App(f, e) => interpCPS(f, env, {
      fv => fv match {
        case CloV(p, b, fenv) => interpCPS(e, env, {
          av => interpCPS(b, fenv + (p -> av), k)
        })
        case v => error("not a function")
      }
    })

  }

  def reduce(k: Cont, s: Stack): (Cont, Stack) = (k, s) match {
    case (EvalK(env, expr, k), s) => expr match {
      case Num(n) => (k, NumV(n) :: s)
      case Id(id) => {
        env.get(id) match {
          case Some(v) => (k, v :: s)
          case None => error("free identifier")
        }
      }
      case Fun(p, b) => (k, CloV(p, b, env) :: s)
      case Add(l, r) => (EvalK(env, l, EvalK(env, r, AddK(k))), s)
      case Mul(l, r) => (EvalK(env, l, EvalK(env, r, MulK(k))), s)
      case App(f, e) => (EvalK(env, f, EvalK(env, e, AppK(k))), s)
    }
    case (AddK(k), NumV(rn) :: NumV(ln) :: s) => (k, NumV(ln + rn) :: s)
    case (AddK(k), _ :: _ :: s) => error("invalid operation")
    case (MulK(k), NumV(rn) :: NumV(ln) :: s) => (k, NumV(ln * rn) :: s)
    case (MulK(k), _ :: _ :: s) => error("invalid operation")
    case (AppK(k), a :: CloV(p, b, env) :: s) => (EvalK(env + (p -> a), b, k), s)
    case (AppK(k), _ :: _ :: s) => error("not a function")
    case _ => error("idk")
  }

}
