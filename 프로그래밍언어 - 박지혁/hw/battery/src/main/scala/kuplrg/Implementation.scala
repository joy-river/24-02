package kuplrg

object Implementation extends Template {

  import Expr.*
  import RecDef.*
  import Value.*
  import Type.*
  import TypeInfo.*

  def typeCheck(expr: Expr, tenv: TypeEnv): Type = expr match {
    case EUnit => UnitT
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
      if(isSame(typeCheck(e1, tenv), typeCheck(e2, tenv))) BoolT
      else error("type error")
    }
    case ELt(e1, e2) => {
      (typeCheck(e1, tenv), typeCheck(e2, tenv)) match {
        case (NumT, NumT) => BoolT
        case _ => error("type error")
      }
    }
    case ESeq(e1, e2) => {
      val t1 = typeCheck(e1, tenv)
      typeCheck(e2, tenv)
    }
    case EIf(e0, e1, e2) => {
      (typeCheck(e0, tenv)) match {
        case BoolT => {
          val t1 = typeCheck(e1, tenv)
          val t2 = typeCheck(e2, tenv)
          if(isSame(t1, t2)) t1 else error("type error")
        }
        case _ => error("type error")
      }
    }
    case EVal(x, tyOpt, e1, e2) => {
      if (tyOpt.isEmpty) 
      then typeCheck(e2, tenv.addVar(x, typeCheck(e1, tenv)))
      else {
        mustSame(tyOpt.get, typeCheck(e1, tenv))
        typeCheck(e2, tenv.addVar(x, tyOpt.get))
      }
    }
    case EFun(params, e0) => {
      val ptys = params.map(_.ty)
      for (pty <- ptys) mustValid(pty, tenv)
      val rty = typeCheck(e0, tenv.addVars(params.map(p => p.name -> p.ty)))
      ArrowT(List(), ptys, rty)
    }
    case EApp(fun, tys, args) => {
      for (ty <- tys) mustValid(ty, tenv)
      (typeCheck(fun, tenv)) match {
        case ArrowT(tvars, paramTys, retTy) => {
          val atys = args.map(arg => typeCheck(arg, tenv))
          if (atys.length != paramTys.length) error("Argument mismatch")
          for (aty <- atys; pty <- paramTys) mustSame(aty, subst(pty, tvars, tys))
          retTy
        }
      }
    }

  }

  def mustValid(ty : Type, tEnv: TypeEnv): Type = ty match {
    case NumT | BoolT | StrT | UnitT => ty
    case ArrowT(tvars, paramTys, retTy) => {
      val newtEnv = tEnv.addTypeVars(tvars)
      for (paramTy <- paramTys) mustValid(paramTy, newtEnv)
      mustValid(retTy, newtEnv)
      ArrowT(tvars, paramTys, retTy)
    }
    case IdT(name, tys) => if (!tEnv.tys.contains(name)) error("Type unsound")
      IdT(name, tys.map(ty => mustValid(ty, tEnv)))
  }

  def isSame(lty : Type, rty: Type): Boolean = (lty, rty) match {
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (StrT, StrT) => true
    case (UnitT, UnitT) => true
    case (IdT(lname, ltys), IdT(rname, rtys)) => {
      if (lname != rname) false
      else if (ltys.length != rtys.length) false
      else {
        for (lty <- ltys; rty <- rtys) if (!isSame(lty, rty)) false
        true
      }
    }
    case (ArrowT(ltvars, lparamTys, lretTy), ArrowT(rtvars, rparamTys, rretTy)) => {
      if (ltvars.length != rtvars.length) false
      else {
        for (ltvar <- ltvars; rtvar <- rtvars) if (ltvar != rtvar) false
        if (lparamTys.length != rparamTys.length) false
        else {
          for (lparamTy <- lparamTys; rparamTy <- rparamTys) if (!isSame(lparamTy, rparamTy)) false
          isSame(lretTy, rretTy)
        }
      }
    }
    case _ => false
  }
  
  def mustSame(lty: Type, rty: Type): Unit = if (!isSame(lty, rty)) error("Type error")

  def subst(bodyTy: Type, tvars: List[String], tys: List[Type]): Type = bodyTy match {
    case UnitT | NumT | BoolT | StrT => bodyTy
    case ArrowT(tvars1, paramTys, retTy) =>
      ArrowT(
        tvars1, 
        paramTys.map(paramTy => subst(paramTy, tvars, tys)),
        subst(retTy, tvars, tys)
      )
    case IdT(name2, tys2) =>
      if (tvars.contains(name2)) tys(tvars.indexOf(name2))
      else IdT(name2, tys2.map(ty => subst(ty, tvars, tys))) 
  }




  def interp(expr: Expr, env: Env): Value = ???
}
