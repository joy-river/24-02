package kuplrg

object Implementation extends Template {
  import Expr.*
  import Value.*
  
  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def interp(expr: Expr, env: Env): Value = 
    expr match {
    case ENum(n) => NumV(n)
    case EBool(b) => BoolV(b)
    case EId(name) => env.get(name) match {
      case Some(v) => v
      case _ => error("free identifier")
    }
    case EAdd(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 + n2)
        case _ => error("invalid operation")
      }
    }
    case EMul(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)
      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => NumV(n1 * n2)
        case _ => error("invalid operation")
      }
    }
    case EDiv(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)

      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => 
          if n2 != 0 then NumV(n1 / n2) else error("invalid operation")
        case _ => error("invalid operation")
      }
    }

    case EMod(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)

      (v1, v2) match {
        case (NumV(n1), NumV(n2)) => 
          if n2 != 0 then NumV(n1 % n2) else error("invalid operation")
        case _ => error("invalid operation")
      }
    }
    
    case EEq(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)

      BoolV(eq(v1, v2)) 
    }

    case ELt(e1, e2) => {
      val v1 = interp(e1, env)
      val v2 = interp(e2, env)

      (v1, v2) match {
          case (NumV(n1), NumV(n2)) => BoolV(n1 < n2)
          case _ => error("invalid operation")
      }
    }

    case EIf(c, te, ee) => {
      val Cond = interp(c, env)

      Cond match {
        case BoolV(b) => {
          if b then interp(te, env) else interp(ee, env)
        }
        case _ => error("not a boolean")
      }
    }

    case ENil => NilV

    case ECons(h, t) => {
      val v1 = interp(h, env)
      val v2 = interp(t, env)
      
      (v1, v2) match {
        case (CloV(p, b, e), _) => error("not a list")
        case (_, CloV(p, b, e)) => error("not a list")
        case (BoolV(b), _) => error("not a list")
        case (_, BoolV(b)) => error("not a list")
        case (_, _) => ConsV(v1, v2)
      }
      
    }

    case EHead(l) => {
      val v = interp(l, env)

      v match {
        case ConsV(h, t) => h
        case NilV => error("empty list")
        case _ => error("not a list")
      }
    }

    case ETail(l) => {
      val v = interp(l, env)

      v match {
        case ConsV(h, t) => t
        case NilV => error("empty list")
        case _ => error("not a list")
      }
    }

    case EMap(le, fe) => {
      val v1 = interp(le, env)
      val v2 = interp(fe, env)

      map(v1, v2)
    }

    case EFlatMap(le, fe) => {
      val v1 = interp(le, env)
      val v2 = interp(fe, env)

      join(map(v1, v2))
    }

    case EFilter(le, fe) => {
      val v1 = interp(le, env)
      val v2 = interp(fe, env)

      filter(v1, v2)
    } 
  
    case EFoldLeft(le, ie, fe) => {
      val v1 = interp(le, env)
      val v2 = interp(ie, env)
      val v3 = interp(fe, env)

      foldLeft(v1, v2, v3)
    }

    case ETuple(le) => {
      TupleV(le.map(e => interp(e, env)))
    }

    case EProj(t, idx: Int) => {
      val v1 = interp(t, env)

      v1 match {
        case TupleV(values) => 
          if values.length > idx - 1 then values(idx - 1) else error("out of bounds")
        case _ => error("not a tuple")
      }

    }

    case EVal(n, v, s) => {
      val v1 = interp(v, env)
      val newEnv = env + (n -> v1)
      
      interp(s, newEnv)
    }

    case ERec(d, s) => {
      lazy val newEnv : Env = env ++ d.map(f => f.name -> CloV(f.params, f.body, () => newEnv))

      interp(s, newEnv)
    }

    case EFun(p, b) => CloV(p, b, () => env)
    
    case EApp(f, a) => {
      val v1 = interp(f, env)
      val v2 = a.map(e => interp(e, env))
      app(v1, v2)
    }

  }

  def eq(left: Value, right: Value): Boolean = {
    (left, right) match {
      case (NumV(n1), NumV(n2)) => n1 == n2
      case (BoolV(b1), BoolV(b2)) => b1 == b2
      case (ConsV(h1, t1), ConsV(h2, t2)) => eq(h1, h2) && eq(t1, t2)
      case (NilV, NilV) => true
      case (NilV, ConsV(h1, t1)) => false
      case (ConsV(h1, t1), NilV) => false
      case (_, _) => false
    }
  }

  def map(list: Value, func: Value): Value = {
    (list, func) match {
      case (NilV, _) => NilV
      case (ConsV(h, t), CloV(p, b, e)) => ConsV(app(func, List(h)), map(t, func))
      case (ConsV(h, t), _) => error("not a function")
      case (_, CloV(p, b, e)) => error("not a list")
    }
  }
  
  def join(list: Value): Value = {
    list match {
      case (NilV) => NilV
      case (ConsV(h, t)) => h match {
        case NilV => join(t)
        case ConsV(h1, t1) => ConsV(h1, join(ConsV(t1, t)))
        case _ => error("not a list")
      }
      case (_) => error("not a list")
    }
  }

  def filter(list: Value, func: Value): Value = {
    (list, func) match {
      case (NilV, _) => NilV
      case (ConsV(h, t), CloV(p, b, e)) => {
        val is_bool = app(func, List(h))

        is_bool match {
          case BoolV(true) => ConsV(h, filter(t, func))
          case BoolV(false) => filter(t, func)
          case _ => error("not a boolean")
        }
      }
      case (ConsV(h, t), _) => error("not a function")
      case (_, CloV(p, b, e)) => error("not a list")
    }
  }

  def foldLeft(list: Value, init: Value, func: Value): Value = {
    (list, func) match {
      case (NilV, _) => init
      case (ConsV(h, t), CloV(p, b, e)) => foldLeft(t, app(func, List(init, h)), func)
      case (ConsV(h, t), _) => error("not a function")
      case (_, CloV(p, b, e)) => error("not a list")
    }
  }

  def app(func: Value, args: List[Value]): Value = {
    func match {
      case (CloV(p, b, e)) => {
        if p.length == args.length then {
          val env = e()
          val newEnv = env ++ p.zip(args).toMap
          interp(b, newEnv)
        }
        else error("arity mismatch")
      }
      case (_) => error("not a function") 
  }
  }
  // ---------------------------------------------------------------------------
  // Problem #2
  // -------------------------------------------------------- -------------------

  def subExpr1: String = """ list <- lists; 
                          num <- list;
                          if pred(num);
                          """
  def subExpr2: String = """num * num"""


}