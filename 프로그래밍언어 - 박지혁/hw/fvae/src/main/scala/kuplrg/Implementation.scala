package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env): Value = {
    expr match {
      case Num(n) => NumV(n)
      case Val(id, e1, e2) => {
        val v1 = interp(e1, env)
        val newEnv = env + (id -> v1)
        interp(e2, newEnv)
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
      case Id(id) => {
        env.get(id) match {
          case Some(v) => v
          case _ => error("free identifier")
        }
      }
      case Fun(p, b) => CloV(p, b, env)

      case App(f, e1) => {
        val func = interp(f, env)
        val arg = interp(e1, env)
        func match {
          case CloV(p, b, e) => {
            val newEnv = e + (p -> arg)
            interp(b, newEnv)
          }
          case _ => error("not a function")
        }
      }
   }
  }

  def interpDS(expr: Expr, env: Env): Value = {
    expr match {
      case Num(n) => NumV(n)
      case Add(e1, e2) => {
        val v1 = interpDS(e1, env)
        val v2 = interpDS(e2, env)
        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
          case _ => error("invalid operation")
        }
      }
      case Mul(e1,e2) => {
        val v1 = interpDS(e1, env)
        val v2 = interpDS(e2, env)
        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
          case _ => error("invalid operation")
        }
      }
      case Val(id, e1, e2) => {
        val v1 = interpDS(e1, env)
        val newEnv = env + (id -> v1)
        interpDS(e2, newEnv)
      }
      case Id(id) => env.get(id) match {
        case Some(v) => v
        case _ => error("free identifier")
      }
      case Fun(p, b) => CloV(p, b, env)
      case App(f, e1) => {
        val func = interpDS(f, env)
        val arg = interpDS(e1, env)

        func match {
          case CloV(p, b, e) => {
            val newEnv = env + (p -> arg)
            interpDS(b, newEnv)
          }
          case _ => error("not a function")
        }

      }
    }
  }
}
