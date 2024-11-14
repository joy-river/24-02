package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Cont.*

  def reduce(k: Cont, s: Stack): (Cont, Stack) = k match{
    case EmptyK => (k, s)
    case EvalK(e, env, k) => e match {
      case Num(n) => (k, NumV(n) :: s)
      case Add(l, r) => (EvalK(l, env, EvalK(r, env, AddK(k))), s)
      case Mul(l, r) => (EvalK(l, env, EvalK(r, env, MulK(k))), s)
      case Id(id) => (k, env.getOrElse(id, error("free identifier")) :: s)
      case Fun(p, b) => (k, CloV(p, b, env) :: s)
      case App(f, e) => (EvalK(f, env, EvalK(e, env, AppK(k))), s)
      case Vcc(x, b) => (EvalK(b, env + (x -> ContV(k, s)), s), s)
    }
    case AddK(k) => {
      s match {
        case NumV(rn) :: Num
      }
    }
  }
}
