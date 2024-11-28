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
      typeCheck(e2, tenv.addVar(x, t1))
    }
    case Id(x) => tenv.vars.get(x) match {
      case Some(t) => t
      case None => error("undefined variable")
    }
    case Fun(params, e) => {
      params.foreach(p => if (!wellFormed(p.ty, tenv))
        then error("Not well-formed")
      )
      typeCheck(e, tenv.addVars(params.map(p => p.name -> p.ty)))
    }
    case Rec(x0, params, rty, e0, e1) => {
      params.foreach(p => if (!wellFormed(p.ty, tenv)) then error("Not well-formed"))
      if (!wellFormed(rty, tenv)) then error("Not well-formed")

      val t1 = typeCheck(e0, tenv.addVars(params.map(p => p.name -> p.ty)).addVar(x0, ArrowT(params.map(_.ty), rty)))
      if t1 == rty then typeCheck(e1, tenv.addVar(x0, ArrowT(params.map(_.ty), rty))) else error("Type error")
    }
    case App(e0, args) => {
      val t0 = typeCheck(e0, tenv)
      t0 match {
        case ArrowT(params, rty) => {
          if (args.length != params.length) then error("Argument mismatch")
          args.zip(params).foreach {
            case (arg, param) => {
              val at = typeCheck(arg, tenv)
              if(at != param) then error("Type mismatch")
            }
            case _ => error("Type mismatch")
          }
          rty
        }
        case _ => error("Type error")
      }
    }
    case If(e0, e1, e2) => {
      val t0 = typeCheck(e0, tenv)
      val t1 = typeCheck(e1, tenv)
      val t2 = typeCheck(e2, tenv)
      (t0, t1, t2) match {
        case (BoolT, t1, t2) => if t1 == t2 then t1 else error("Type error")
        case _ => error("Type error")
      }
    }
    case TypeDef(t, varts, e) => {
      
    }

  }
  def wellFormed(t: Type, tEnv: TypeEnv): Boolean = t match{
    case NumT | BoolT => true
    case ArrowT(params, rety) => params.forall(wellFormed(_, tEnv)) && wellFormed(rety, tEnv)
    case NameT(name) => tEnv.vars.get(name) match {
      case Some(t) => wellFormed(t, tEnv)
      case None => error("undefined variable")
    }
  }
  def interp(expr: Expr, env: Env): Value = ???
}
