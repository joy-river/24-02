package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match {
    case EUnit() => UnitT
    case ENum(_) => NumT
    case EBool(_) => BoolT
    case EStr(_) => StrT
    case EId(x) => tenv.vars.get(x) match {
      case Some(t) => t
      case None => error("free identifier : TypeCheck")
    }
    case EAdd(e1, e2) => {
      (typeCheck(e1, tenv), typeCheck(e2, tenv)) match {
        case (NumT, NumT) => NumT
        case _ => error("type error")
      }
    }
    case EMul(e1, e2) => {
      (typeCheck(e1, tenv), typeCheck(e2, tenv)) match {
        case (NumT, NumT) => NumT
        case _ => error("type error")
      }
    }
    case EDiv(e1, e2) => {
      (typeCheck(e1, tenv), typeCheck(e2, tenv)) match {
        case (NumT, NumT) => NumT
        case _ => error("type error")
      }
    }
    case EMod(e1, e2) => {
      (typeCheck(e1, tenv), typeCheck(e2, tenv)) match {
        case (NumT, NumT) => NumT
        case _ => error("type error")
      }
    }
    case EConcat(e1, e2) => {
      (typeCheck(e1, tenv), typeCheck(e2, tenv)) match {
        case (StrT, StrT) => StrT
        case _ => error("type error")
      }
    }
    case EEq(e1, e2) => {
      isSame
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



  def interp(expr: Expr, env: Env): Value = ???
}
