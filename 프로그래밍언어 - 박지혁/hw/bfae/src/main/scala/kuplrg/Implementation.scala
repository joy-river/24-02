package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = {
    expr match {
      case Num(n) => (NumV(n), mem) 

      case Add(e1, e2) => {
        val (v1, ml) = interp(e1, env, mem)
        val (v2, mr) = interp(e2, env, ml)
        
        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), mr)
          case _ => error("invalid operation")
        }
      }

      case Mul(e1, e2) => {
        val (v1, ml) = interp(e1, env, mem)
        val (v2, mr) = interp(e2, env, ml)
        
        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => (NumV(n1 * n2), mr)
          case _ => error("invalid operation")
        }
      }

      case Id(id) => {
        env.get(id) match {
          case Some(v) => (v, mem)
          case None => error("free identifier")
        }
      }

      case Fun(p, b) => (CloV(p, b, env), mem)

      case App(f, a) => {
        val (v1, m1) = interp(f, env, mem)
        val (v2, m2) = interp(a, env, m1)

        v1 match {
          case CloV(p, b, e) => interp(b, e + (p -> v2), m2)
          case _ => error("not a function")
        }

      }

      case NewBox(c) => {
        val (v1, m1) = interp(c, env, mem)
        val address = m1.keySet.maxOption.fold(0)(_ + 1)

        (BoxV(address), m1 + (address -> v1))
      }

      case GetBox(e) => {
        val (v1, m1) = interp(e, env, mem)

        v1 match {
          case BoxV(a) => (m1.getOrElse(a, error("invalid address")), m1)
          case _ => error("not a box")
        }
      }

      case SetBox(e1, e2) => {
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)

        v1 match {
          case BoxV(a) => (v2, m2.updated(a, v2))
          case _ => error("not a box")
        }
      }
      
      case Seq(le, re) => {
        val (v1, m1) = interp(le, env, mem)
        interp(re, env, m1)
      }
    }
  }
}