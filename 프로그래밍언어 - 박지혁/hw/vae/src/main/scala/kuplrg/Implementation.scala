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

  def bindingIds(expr: Expr): Set[String] = {
    expr match {
      case Num(_) => Set.empty
      case Add(e1, e2) => bindingIds(e1) ++ bindingIds(e2)
      case Mul(e1, e2) => bindingIds(e1) ++ bindingIds(e2)
      case Id(id) => Set.empty
      case Val(id, e1, e2) => Set(id) ++ bindingIds(e1) ++ bindingIds(e2)
    }
  }

  def boundIds(expr: Expr): Set[String] = {
    expr match {
      case Num(_) => Set.empty
      case Add(e1, e2) => boundIds(e1) ++ boundIds(e2)
      case Mul(e1, e2) => boundIds(e1) ++ boundIds(e2)
      case Id(id) => Set.empty
      case Val(id, e1, e2) =>{
        val e1BoundIds = boundIds(e1)
        val freeInE2 = freeIds(e2)
        val e2BoundIds = e1BoundIds ++ boundIds(e2)
        
        if (freeInE2.contains(id)) Set(id) ++ e2BoundIds else e2BoundIds
      }
    }
  }
  
  def shadowedIds(expr: Expr): Set[String] = {
    expr match {
      case Num(_) => Set.empty
      case Add(e1, e2) => shadowedIds(e1) ++ shadowedIds(e2)
      case Mul(e1, e2) => shadowedIds(e1) ++ shadowedIds(e2)
      case Id(id) => Set.empty
      case Val(id, e1, e2) => {
        val e1ShadowedIds = shadowedIds(e1)
        val e2Binding = bindingIds(e2)
        val e2ShadowedIds = e1ShadowedIds ++ shadowedIds(e2)

        if (e2Binding.contains(id)) Set(id) ++ e2ShadowedIds else e2ShadowedIds
    }
  }
  }
}
