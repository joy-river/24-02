package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env): Value = {
    expr match {
      case Num(n) => n
      case Add(e1, e2) => interp(e1, env) + interp(e2, env)
      case Mul(e1, e2) => interp(e1, env) * interp(e2, env)
      case Val(id, e1, e2) => {
        val v1 = interp(e1, env)
        val env2 = env + (id -> v1)
        interp(e2, env2)
      }
      case Id(id) => env.get(id) match {
        case Some(v) => v
        case _ => error("free identifier")
      }
    }
  }

  def freeIds(expr: Expr): Set[String] = {
    expr match {
      case Num(_) => Set.empty
      case Add(e1, e2) => freeIds(e1) ++ freeIds(e2)
      case Mul(e1, e2) => freeIds(e1) ++ freeIds(e2)
      case Id(id) => Set(id)
      case Val(id, e1, e2) => freeIds(e1) ++ (freeIds(e2) - id) 
    }
  }

  def bindingIds(expr: Expr): Set[String] = ???

  def boundIds(expr: Expr): Set[String] = ???

  def shadowedIds(expr: Expr): Set[String] = ???

}
