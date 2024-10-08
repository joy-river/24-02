package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def interp(expr: Expr, env: Env): Value = expr match {
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
      
      ConsV(v1, v2)
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

      (v1, v2) match {
        case (ConsV(h, t), CloV(p, b, e)) => filter(v1, v2)
        case (ConsV(h, t), _) => error("not a function")
        case (_, CloV(p, b, e)) => error("not a list")
      }
    } 
  
    case EFoldLeft(le, ie, fe) => {
      val v1 = interp(le, env)
      val v2 = interp(ie, env)
      val v3 = interp(fe, env)

      (v1, v2, v3) match {
        case (ConsV(h, t), _, CloV(p, b, e)) => foldLeft(v1, v2, v3)
        case (ConsV(h, t), _, _) => error("not a function")
        case (_, _ , CloV(p, b, e)) => error("not a list")
      }
    }

    //  tuple
    // case ETuple(exprs: List[Expr])
    //  tuple projection
    // case EProj(tuple: Expr, index: Int)
    //  variable definition
    // case EVal(name: String, value: Expr, scope: Expr)
    //  lambda function
    // case EFun(params: List[String], body: Expr)
    //  mutually recursive function
    // case ERec(defs: List[FunDef], scope: Expr)
    //  function application
    // case EApp(fun: Expr, args: List[Expr])

    case EFun(p, b) => CloV(p, b, () => env)

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
        case _ => ConsV(h, join(t)) 
      }
      case (_) => error("not a list")
    }
  }

  def filter(list: Value, func: Value): Value = {
    (list, func) match {
      case (NilV, _) => NilV
      case (ConsV(h, t), CloV(p, b, e)) => {
        if eq(app(func, List(h)), BoolV(true)) 
        then ConsV(h, filter(t, func)) 
        else filter(t, func)
      }
      case (ConsV(h, t), _) => error("not a function")
      case (_, CloV(p, b, e)) => error("not a list")
    }
  }

  def foldLeft(list: Value, init: Value, func: Value): Value = ???

  def app(func: Value, args: List[Value]): Value = ???

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def subExpr1: String = ???

  def subExpr2: String = ???

}