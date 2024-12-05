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
          subst(retTy, tvars, tys)
        }
        case _ => error("Type error")
      }
    }

  case ERecDefs(defs, e0) => {
    // Step 1: 타입 환경 확장
    lazy val tEnv2: TypeEnv = {
      defs.foldLeft(tenv) { (env, defn) =>
        defn match {
          case LazyVal(x, t, _) =>
            println("LazyVal 추가: " + x + " -> " + t + "\n현재 환경:\n" + env)
            env.addVar(x -> t)

          case RecFun(x, tvars, params, rty, _) =>
            val funcType = ArrowT(tvars, params.map(_.ty), rty)
            println("RecFun 추가: " + x + " -> " + funcType + "\n현재 환경:\n" + env)
            env.addVar(x -> funcType)

          case TypeDef(x, tvars, varts) =>
            if (env.tys.contains(x)) error("Type Error: recDefs")
            val adtConstructors = varts.map { vart =>
              vart.name -> ArrowT(tvars, vart.params.map(_.ty), IdT(x, tvars.map(IdT(_))))
            }.toMap
            println("TypeDef 추가: " + x + "\n현재 환경:\n" + env)
            env.addTypeName(x, tvars, varts).addVars(adtConstructors)
        }
      }
    }

    println("최종 환경:\n" + tEnv2)
    // Step 2: 타입 규칙 검증
    defs.foreach { defn =>
      defn match {
        case LazyVal(x, t, e) => {
          mustValid(t, tEnv2) // 확장된 환경에서 타입 유효성 검사
          val exprType = typeCheck(e, tEnv2)
          if (isSame(t, exprType)) println(s"LazyVal $x 검증 성공") else error("Type error")
        }

        case RecFun(x, tvars, params, rty, e) => {
          for (tvar <- tvars) if (tEnv2.vars.contains(tvar)) error("Type unsound : typeRule")

          val newtEnv = tEnv2.addTypeVars(tvars)
          val ptys = params.map(_.ty)

          for (pty <- ptys) mustValid(pty, newtEnv)
          mustValid(rty, newtEnv)

          val fty = ArrowT(tvars, ptys, rty)
          val bty = typeCheck(e, newtEnv.addVar(x, fty))
          mustSame(bty, rty)

          println(s"RecFun $x 검증 성공")
        }

        case TypeDef(x, tvars, varts) => {
          for (tvar <- tvars) if (tEnv2.vars.contains(tvar)) error("Type unsound : typeRule")

          val newtEnv = tEnv2.addTypeVars(tvars)
          for (vart <- varts; param <- vart.params) mustValid(param.ty, newtEnv)

          println(s"TypeDef $x 검증 성공")
        }
      }
    }

    // Step 3: 최종 표현식 타입 검사
    val t1 = typeCheck(e0, tEnv2)
    mustValid(t1, tenv)
    t1
  }


    case EMatch(e0, mcases) => {
      (typeCheck(e0, tenv)) match {
        case IdT(name, tys) => {
          tenv.tys.get(name) match {
            case Some(TIAdt(tvars, variants)) => {
              if(tvars.length != tys.length) error("invalid match")
              val tmap: Map[String, List[Type]] = variants.map { case (constrName, params) =>
                constrName -> params.map(p => mustValid(subst(p.ty, tvars, tys), tenv))
              }.toMap
              mustValidMatch(mcases, tmap)
              val tys2 = for (MatchCase(x, ps, b) <- mcases)
                yield typeCheck(b, tenv.addVars((ps zip tmap(x))))
              tys2.reduce((lty, rty) => { mustSame(lty, rty); lty })
            }
            case None => error("invalid match")
          }
        }
      }
    }

    case EExit(t, e) => (typeCheck(e, tenv)) match {
      case StrT => {
        mustValid(t, tenv) 
        t
      }
      case _ => error("invalid exit type")
    }

  }

  
  def interp(expr: Expr, env: Env): Value = expr match {
    case EUnit => UnitV
    case ENum(n) => NumV(n)
    case EBool(b) => BoolV(b)
    case EStr(s) => StrV(s)
    case EId(x) => {
      env.get(x) match {
        case Some(v) => v match {
          case ExprV(e, env2) => interp(e, env2())
          case _ => v
        }
        case None => error("free identifier : Interp")
      }
    }
    case EAdd(e1, e2) => (interp(e1, env), interp(e2, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
      case _ => error("invalid operation")
    }
    case EMul(e1, e2) => (interp(e1, env), interp(e2, env)) match {
      case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
      case _ => error("invalid operation")
    }
    case EDiv(e1, e2) => (interp(e1, env), interp(e2, env)) match {
      case (NumV(n1), NumV(n2)) => if (n2 == 0) error("invalid operation") else NumV(n1 / n2)
      case _ => error("invalid operation")
    }
    case EMod(e1, e2) => (interp(e1, env), interp(e2, env)) match {
      case (NumV(n1), NumV(n2)) => if (n2 == 0) error("invalid operation") else NumV(n1 % n2)
      case _ => error("invalid operation")
    }
    case EEq(e1, e2) => eq(interp(e1, env), interp(e2, env))
    case ELt(e1, e2) => (interp(e1, env), interp(e2, env)) match {
      case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
      case _ => error("invalid operation")
    }
    case EConcat(e1, e2) => (interp(e1, env), interp(e2, env)) match {
      case (StrV(s1), StrV(s2)) => StrV(s1 + s2)
      case _ => error("invalid operation")
    }
    case ESeq(e1, e2) => {
      val v1 = interp(e1, env)
      interp(e2, env)
    }
    case EIf(e0, e1, e2) => interp(e0, env) match {
      case BoolV(true) => interp(e1, env)
      case BoolV(false) => interp(e2, env)
      case _ => error("invalid condition")
    }
    case EVal(x, _, e1, e2) => {
      interp(e2, env + (x -> interp(e1, env)))
    }
    case EFun(params, e0) => CloV(params.map(p => p.name), e0, () => env)
    case EApp(e0, tys, args) => (interp(e0, env)) match {
      case CloV(params, body, env2) => {
        interp(body, env2() ++ params.zip(args.map(arg => interp(arg, env))))
      }
      case ConstrV(name) => VariantV(name, args.map(arg => interp(arg, env)))
      case _ => error("not a function or constructor")
    }
    case ERecDefs(defs, e0) => {
      lazy val env2: Env = env ++ defs.flatMap {
        case LazyVal(x, t, e) => Some(x -> ExprV(e, () => env2))
        case RecFun(x, tvars, params, rty, e) => Some(x -> CloV(params.map(_.name), e, () => env2))
        case TypeDef(x, tvars, varts) => varts.map(v => v.name -> ConstrV(v.name))
      }
      interp(e0, env2)
    }

    case EMatch(e0, mcases) => (interp(e0, env)) match {
      case VariantV(name, args) => {
        mcases.find(mcase => mcase.name == name) match {
          case Some(mcase) => interp(mcase.body, env ++ mcase.params.zip(args))
          case None => error("no matching case")
        }
      }
      case _ => error("not a variant")
    }
    case EExit(_, _ ) => error("exit by EExit")
  }
  def eq(v1: Value, v2: Value): Value = (v1, v2) match {
    case (UnitV, UnitV) => BoolV(true)
    case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
    case (BoolV(b1), BoolV(b2)) => BoolV(b1 == b2)
    case (StrV(s1), StrV(s2)) => BoolV(s1 == s2)
    case (VariantV(name1, args1), VariantV(name2, args2)) =>
      if (name1 == name2 && args1.zip(args2).forall { case (a1, a2) => eq(a1, a2) == BoolV(true) })
        BoolV(true)
      else
        BoolV(false)
    case _ => BoolV(false)
  }



  def mustValid(ty : Type, tEnv: TypeEnv): Type = ty match {
    case NumT | BoolT | StrT | UnitT => ty
    case ArrowT(tvars, paramTys, retTy) => {
      val newtEnv = tEnv.addTypeVars(tvars)
      for (paramTy <- paramTys) mustValid(paramTy, newtEnv)
      mustValid(retTy, newtEnv)
      ArrowT(tvars, paramTys, retTy)
    }
    case IdT(name, tys) => tEnv.tys.getOrElse(name, error("Type Error: mustValid" + "\n" + name + "\n"+ tEnv))
      IdT(name, tys.map(ty => mustValid(ty, tEnv)))
  }

  def isSame(lty: Type, rty: Type): Boolean = (lty, rty) match {
    case (NumT, NumT) => true
    case (BoolT, BoolT) => true
    case (StrT, StrT) => true
    case (UnitT, UnitT) => true
    case (IdT(lname, ltys), IdT(rname, rtys)) =>
      if (lname != rname) false
      else if (ltys.length != rtys.length) false
      else {
        val same = ltys.zip(rtys).forall { case (lty, rty) => isSame(lty, rty) }
        if (same) true else false
      }
    case (ArrowT(ltvars, ltys, lrty), ArrowT(rtvars, rtys, rrty)) =>
      if (ltvars.length != rtvars.length) false
      else if (ltys.length != rtys.length) false
      else {
        val sameParams = ltys.zip(rtys).forall { case (lty, rty) => isSame(lty, rty) }
        val sameReturn = isSame(lrty, rrty)
        if (sameParams && sameReturn) true else false
      }
    case _ => false
  }
  
  def mustSame(lty: Type, rty: Type): Unit = if (!isSame(lty, rty)) error("Type error")

  def subst(bodyTy: Type, tvars: List[String], tys: List[Type]): Type = bodyTy match {
    case UnitT | NumT | BoolT | StrT => bodyTy
    case ArrowT(tv, paramTys, retTy) => 
    ArrowT(tv, paramTys.map(paramTy => subst(paramTy, tvars, tys)), subst(retTy, tvars, tys))
    case IdT(name, tys2) =>
      if (tvars.contains(name)) tys2(tvars.indexOf(name))
      else IdT(name, tys2.map(ty => subst(ty, tvars, tys))) 
  }

  def mustValidMatch(cs: List[MatchCase], tmap: Map[String, List[Type]]): Unit = {
    val xs = cs.map(_.name).toSet
    if (xs.size != cs.size) error("invalid match") // duplicate cases
    if (tmap.keySet != xs) error("invalid match") // non-exhaustive cases
    for (MatchCase(x, ps, _) <- cs if (tmap(x).size != ps.size))
    error("invalid match") 
  }
}

