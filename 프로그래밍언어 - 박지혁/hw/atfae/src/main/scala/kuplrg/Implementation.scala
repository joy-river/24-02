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
      typeCheck(Id(t), tenv)
      val newtEnv = tenv.addType(t, varts.map(vart => vart.name -> vart.ptys).toMap)
      if (!varts.forall(vart => vart.ptys.forall(pty => wellFormed(pty, newtEnv))))
      then error("Type error")
      varts.foreach( vart =>
        newtEnv.addVar(vart.name, ArrowT(vart.ptys, NameT(t)))
      )
      val rty = typeCheck(e, newtEnv)
      if (!wellFormed(rty, tenv)) then error("Type error")
      rty
    }

    case Match(e, mcases) => typeCheck(e, tenv) match{
      case NameT(name) => {
        val tmap = tenv.tys.getOrElse(name, error("Type error"))
        mustValidMatch(mcases, tmap)
        val tys = for (MatchCase(x, ps, b) <- mcases)
          yield typeCheck(b, tenv.addVars((ps zip tmap(x))))
        tys.reduce((lty, rty) => { mustSame(lty, rty); lty })
      }
      case _ => error("Type error")
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

  def mustValidMatch(cs: List[MatchCase], tmap: Map[String, List[Type]]): Unit =
    val xs = cs.map(_.name).toSet
    if (xs.size != cs.size) error("invalid match") // duplicate cases
    if (tmap.keySet != xs) error("invalid match") // non-exhaustive cases
    for (MatchCase(x, ps, _) <- cs if (tmap(x).size != ps.size))
      error("invalid match") 

  def mustSame(lty: Type, rty: Type): Unit = lty match {
    case NumT | BoolT => if (lty != rty) error("Type error")
    case ArrowT(ltps, lrty) => rty match {
      case ArrowT(rtps, rrty) => {
        if (ltps.size != rtps.size) error("Type error")
        for ((ltp, rtp) <- (ltps zip rtps)) mustSame(ltp, rtp)
        mustSame(lrty, rrty)
      }
      case _ => error("Type error")
    }
    case NameT(lname) => rty match {
      case NameT(rname) => if (lname != rname) error("Type error")
      case _ => error("Type error")
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
    case Div(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => if (n2 == 0) error("invalid operation") else NumV(n1 / n2)
        case _ => error("invalid operation")
      }
    }
    case Mod(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => if (n2 == 0) error("invalid operation") else NumV(n1 % n2)
        case _ => error("invalid operation")
      }
    }
    case Eq(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => BoolV(n1 == n2)
        case _ => error("invalid operation")
      }
    }
    case Lt(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
        case _ => error("invalid operation")
      }
    }
    
    case Val(x, e1, e2) => {
      val v1 = interp(e1, env)
      interp(e2, env + (x -> v1))
    }

    case Id(x) => env.get(x) match {
      case Some(v) => v
      case None => error("undefined variable")
    }

    case Fun(p, b) => CloV(p.map(p => p.name), b, () => env)

    case Rec(x, params, rty, e0, e1) => {
      lazy val newEnv : Env = env + (x -> CloV(params.map(p => p.name), e0, () => newEnv))
      interp(e1, newEnv)
    }

    case App(f, args) => {
      val argv = args.map(arg => interp(arg, env))
      interp(f, env) match {
        case CloV(params, b, e) => {
          interp(b, e() ++ params.zip(argv))
        }
        case ConstrV(name) => VariantV(name, argv)
        case _ => error("not a function or constructor")
      }
    }

    case If(e0, e1, e2) => interp(e0, env) match {
      case BoolV(true) => interp(e1, env)
      case BoolV(false) => interp(e2, env)
      case _ => error("invalid condition")
    }

    case TypeDef(t, varts, e) => {
      interp(e, env ++ varts.map(vart => vart.name -> ConstrV(vart.name)))
    }

    



  }
}
