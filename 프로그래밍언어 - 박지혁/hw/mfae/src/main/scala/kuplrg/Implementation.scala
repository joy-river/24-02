package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*

  def interp(expr: Expr, env: Env, mem: Mem): (Value, Mem) = {
    expr match {
      case Num(n) => (NumV(n), mem)
      case Add(e1, e2) => {
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)

        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), m2)
          case _ => error("invalid operation")
        }
      }
      case Mul(e1, e2) => {
        val (v1, m1) = interp(e1, env, mem)
        val (v2, m2) = interp(e2, env, m1)

        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => (NumV(n1 * n2), m2)
          case _ => error("invalid operation")
        }
      }
      case Id(id) => {
        val a = env.getOrElse(id, error("free identifier"))
        (mem.getOrElse(a, error("free identifier")), mem)
      }
      case Var(n, i, b) => {
        val (v1, m1) = interp(i, env, mem)
        val a = m1.keySet.maxOption.fold(0)(_ + 1)
        interp(b, env + (n -> a), m1 + (a -> v1))
      }

      case Fun(p, b) => (CloV(p, b, env), mem)

      case App(f, a) => {
        val (v1, m1) = interp(f, env, mem)
        val (v2, m2) = interp(a, env, m1)

        val ads = m2.keySet.maxOption.fold(0)(_ + 1)

        v1 match {
          case CloV(p, b, e) => interp(b, e + (p -> ads), m2 + (ads -> v2))
          case _ => error("not a function")
        }
      }

      case Assign(n, e) => {
        val (v1, m1) = interp(e, env, mem)
        val ads = env.getOrElse(n, error("free identifier"))

        (v1, m1 + (ads -> v1))
      }

      case Seq(le, re) => {
        val (_, m1) = interp(le, env, mem)
        interp(re, env, m1)
      }
    }
  }

  def interpCBR(expr: Expr, env: Env, mem: Mem): (Value, Mem) = {
    expr match {
      case Num(n) => (NumV(n), mem)
      case Add(e1, e2) => {
        val (v1, m1) = interpCBR(e1, env, mem)
        val (v2, m2) = interpCBR(e2, env, m1)

        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => (NumV(n1 + n2), m2)
          case _ => error("invalid operation")
        }
      }
      case Mul(e1, e2) => {
        val (v1, m1) = interpCBR(e1, env, mem)
        val (v2, m2) = interpCBR(e2, env, m1)

        (v1, v2) match {
          case (NumV(n1), NumV(n2)) => (NumV(n1 * n2), m2)
          case _ => error("invalid operation")
        }
      }
      case Id(id) => {
        val a = env.getOrElse(id, error("free identifier"))
        (mem.getOrElse(a, error("free identifier")), mem)
      }
      case Var(n, i, b) => {
        val (v1, m1) = interpCBR(i, env, mem)
        val a = m1.keySet.maxOption.fold(0)(_ + 1)
        interpCBR(b, env + (n -> a), m1 + (a -> v1))
      }

      case Fun(p, b) => (CloV(p, b, env), mem)

      case App(f, a) => {
        val (v1, m1) = interpCBR(f, env, mem)

        v1 match {
          case CloV(p, b, e) => {
            a match {
              case Id(id) => interpCBR(b, e + (p -> env.getOrElse(id, -1)), m1)
              case _ => {
                val (v2, m2) = interpCBR(a, env, m1)
                val newA = m2.keySet.maxOption.fold(0)(_ + 1)
                interpCBR(b, e + (p -> newA), m2 + (newA -> v2))
              }
            }
          }
          case _ => error("not a function")
        }
      }
      case Assign(n, e) => {
        val (v1, m1) = interpCBR(e, env, mem)
        val ads = env.getOrElse(n, error("free identifier"))

        (v1, m1 + (ads -> v1))
      }

      case Seq(le, re) => {
        val (_, m1) = interpCBR(le, env, mem)
        interpCBR(re, env, m1)
      }
    }
  }
}