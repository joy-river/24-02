package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match {
    case Num(_) => NumT
    case Bool(_) => BoolT
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
    case Div(e1, e2) => {
      val t1 = typeCheck(e1, tenv)
      val t2 = typeCheck(e2, tenv)
      (t1, t2) match {
        case (NumT, NumT) => NumT
        case _ => error("type error")
      }
    }
    case Mod(e1, e2) => {
      val t1 = typeCheck(e1, tenv)
      val t2 = typeCheck(e2, tenv)
      (t1, t2) match {
        case (NumT, NumT) => NumT
        case _ => error("type error")
      }
    }
    case Eq(e1, e2) => {
      val t1 = typeCheck(e1, tenv)
      val t2 = typeCheck(e2, tenv)
      (t1, t2) match {
        case (NumT, NumT) => BoolT
        case _ => error("type error")
     }
    }
    case Lt(e1, e2) => {
      val t1 = typeCheck(e1, tenv)
      val t2 = typeCheck(e2, tenv)
      (t1, t2) match {
        case (NumT, NumT) => BoolT
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
    case Rec(x0, x1, t1, t2, e2, e3) => {
      val t = typeCheck(e2, tenv + (x0 -> ArrowT(t1, t2)) + (x1 -> t1))
      val t3 = typeCheck(e3, tenv + (x0 -> ArrowT(t1, t2)))

      if t == t2 then t3 else error("Type error")
    }
    case If(e0, e1, e2) => {
      val t0 = typeCheck(e0, tenv)
      val t1 = typeCheck(e1, tenv)
      val t2 = typeCheck(e2, tenv)
      (t0, t1, t2) match {
        case (BoolT, t3, t4) => if t3 == t4 then t3 else error("Type error")
        case _ => error("Type error")
      }
    }
  }

  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
    case Bool(b) => BoolV(b)
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
      
    case Div(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => {
          if n2 != 0 
          then NumV(n1 / n2) 
          else error("Division by zero")
        }
        case _ => error("Invalid operation")
      }
    }
    case Mod(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => {
          if n2 != 0
          then NumV(n1 % n2) 
          else error("Division by zero")
        }
        case _ => error("Invalid operation")
      }
    }
    case Eq(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
        case _ => error("Invalid operation")
      }
    }
    case Lt(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
        case _ => error("Invalid operation")
      }
    }
    case Val(x, e1, e2) => {
      val v1 = interp(e1, env)
      interp(e2, env + (x -> v1))
    }
    case Id(x) => env.get(x) match {
      case Some(v) => v
      case None => error("Undefined variable")
    }
    case Fun(x, t, e) => CloV(x, e, () => env)
    case Rec(x0, x1, t1, t2, e2, e3) => {
      lazy val newEnv : Env = env + (x0 -> CloV(x1, e2, () =>newEnv))
      interp(e3, newEnv)
    }
    case App(e0, e1) => {
      val v0 = interp(e0, env)
      val v1 = interp(e1, env)
      v0 match {
        case CloV(x, e2, env2) => interp(e2, env2() + (x -> v1))
        case _ => error("Not a function")
      }
    }
    case If(e0, e1, e2) => {
      val v0 = interp(e0, env)
      v0 match {
        case BoolV(b) => if b then interp(e1, env) else interp(e2, env)
        case _ => error("Invalid condition")
      }
    }
  }
}
