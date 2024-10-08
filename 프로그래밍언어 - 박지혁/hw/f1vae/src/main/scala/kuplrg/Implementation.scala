package kuplrg

object Implementation extends Template {

  import Expr.*

  def interp(expr: Expr, env: Env, fenv: FEnv): Value = {
    expr match {
      case Num(n) => n
      case Add(e1, e2) => interp(e1, env, fenv) + interp(e2, env, fenv)
      case Mul(e1, e2) => interp(e1, env, fenv) * interp(e2, env, fenv)
      case Val(id, e1, e2) => {
        val v1 = interp(e1, env, fenv)
        val env2 = env + (id -> v1)
        interp(e2, env2, fenv)
      }
      case Id(id) => env.get(id) match {
        case Some(v) => v
        case _ => error("free identifier")
      }
      case App(f, e1) => fenv.get(f) match {
        case Some(funcdef) => {
          val arg = interp(e1, env, fenv)
          val newenv = Map.empty + (funcdef.param -> arg)
          interp(funcdef.body, newenv , fenv)
        }
        case _ => error("unknown function")
      }

  }
  }

  def interpDS(expr: Expr, env: Env, fenv: FEnv): Value = {
    expr match {
      case Num(n) => n
      case Add(e1, e2) => interpDS(e1, env, fenv) + interpDS(e2, env, fenv)
      case Mul(e1, e2) => interpDS(e1, env, fenv) * interpDS(e2, env, fenv)
      case Val(id, e1, e2) => {
        val v1 = interpDS(e1, env, fenv)
        val env2 = env + (id -> v1)
        interpDS(e2, env2, fenv)
      }
      case Id(id) => env.get(id) match {
        case Some(v) => v
        case _ => error("free identifier")
      }
      case App(f, e1) => fenv.get(f) match {
        case Some(fundef) => {
          val arg = interpDS(e1, env, fenv)
          val newEnv = env + (fundef.param -> arg)
          interpDS(fundef.body, newEnv, fenv)
        }
        case _ => error("unknown function")
      }
  }
}
}