package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def reduce(k: Cont, s: Stack): (Cont, Stack) = k match{
    case EmptyK => (k, s)
    case EvalK(env, e, k) => e match {
      case Num(n) => (k, NumV(n) :: s)
      case Add(l, r) => (EvalK(env, l, EvalK(env, r, AddK(k))), s)
      case Mul(l, r) => (EvalK(env, l, EvalK(env, r, MulK(k))), s)
      case Id(id) => (k, env.getOrElse(id, error("free identifier")) :: s)
      case Fun(p, b) => (k, CloV(p, b, env) :: s)
      case App(f, e) => (EvalK(env, f, EvalK(env, e, AppK(k))), s)
      case Vcc(x, b) => (EvalK(env + (x -> ContV(k, s)), b, k), s)
    }
    case AddK(k) => {
      s match {
        case NumV(rn) :: NumV(ln) :: s => (k, NumV(rn + ln) :: s)
        case _ => error("invalid operation")
      }
    }
    case MulK(k) => {
      s match {
        case NumV(rn) :: NumV(ln) :: s => (k, NumV(rn * ln) :: s)
        case _ => error("invalid operation")
      }
    }
    case AppK(k) => {
      s match {
        case v2 :: CloV(p, b, env) :: s => (EvalK(env + (p -> v2), b, k), s)
        case v2 :: ContV(k2, s2) :: s => (k2, v2 :: s2)
        case _ => error("not a function")
      }
    }
  }
}
