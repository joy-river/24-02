package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Type.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match {
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
      typeCheck(e2, tenv.addVar(x, t1))
    }
    case Id(x) => tenv.vars.get(x) match {
      case Some(t) => t
      case None => error("undefined variable : TypeCheck")
    }
    case Fun(param, t, e) => {
      mustValid(t, tenv)
      val rty = typeCheck(e, tenv.addVar(param, t))

      ArrowT(t, rty)
    }
    case App(f, a) => {
      typeCheck(f, tenv) match {
        case ArrowT(t1, t2) => mustSame(typeCheck(a, tenv), t1) 
            t2
        case _ => error("Type error")
      }
    }

    case TypeAbs(name, e) => {
      if(tenv.tys.contains(name)) error("Type unsound")
      val rty = typeCheck(e, tenv.addType(name))
      PolyT(name, rty)
    }
    
    case TypeApp(e, ty) => {
      typeCheck(e, tenv) match {
        case PolyT(name, rty) => subst(rty, name, mustValid(ty, tenv))
        case t => error("not PolyT")
      }
    }
  }

  def mustValid(ty : Type, tEnv: TypeEnv): Type = ty match {
    case NumT => ty
    case ArrowT(pty, rty) => ArrowT(mustValid(pty, tEnv), mustValid(rty, tEnv))
    case VarT(name) => if (!tEnv.tys.contains(name)) error("Type unsound")
      VarT(name)
    case PolyT(name, ty) => PolyT(name, mustValid(ty, tEnv.addType(name)))
  }

  def isSame(lty : Type, rty: Type): Boolean = (lty, rty) match {
    case (NumT, NumT) => true
    case (ArrowT(lty1, lrty), ArrowT(rty1, rrty)) => isSame(lty1, rty1) && isSame(lrty, rrty)
    case (VarT(lname), VarT(rname)) => lname == rname
    case (PolyT(lname, lrty), PolyT(rname, rrty)) => isSame(lrty, subst(rrty, rname, VarT(lname)))
    case _ => false
  }
  
  def mustSame(lty: Type, rty: Type): Unit = if (!isSame(lty, rty)) error("Type error")

  def subst(bodyTy : Type, name: String, ty: Type): Type = bodyTy match {
    case NumT => NumT
    case ArrowT(paramTy, retTy) => ArrowT(subst(paramTy, name, ty), subst(retTy, name, ty))
    case VarT(name2) => if (name == name2) ty else VarT(name2)
    case PolyT(name2, bodyTy2) => if (name == name2) PolyT(name2, subst(bodyTy2, name, ty)) else PolyT(name2, subst(bodyTy2, name, ty))
  }


  def interp(expr: Expr, env: Env): Value = expr match {
    case Num(n) => NumV(n)
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
    case Val(x, e1, e2) => {
      val v1 = interp(e1, env)
      interp(e2, env + (x -> v1))
    }
    case Id(x) => env.get(x) match {
      case Some(v) => v
      case None => error("undefined variable : Interp")
    }
    case Fun(param, t, e) => CloV(param, e, env)
    case App(f, a) => {
      val v1 = interp(f, env)
      val v2 = interp(a, env)
      v1 match {
        case CloV(param, e, env2) => interp(e, env2 + (param -> v2))
        case _ => error("not a function")
      }
    }

    case TypeAbs(name, e) => TypeAbsV(name, e, env)

    case TypeApp(e, ty) => {
      val v = interp(e, env)
      v match {
        case TypeAbsV(name, e, env2) => interp(e, env2)
        case _ => error("not a type abstraction")
      }
    }
  }

}
