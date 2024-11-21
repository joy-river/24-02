package kuplrg

object Implementation extends Template {

  import Expr.*
  import Value.*
  import Inst.*
  import Control.*

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def reduce(st: State): State =
    val State(k, s, h, mem) = st
    k match {
      case IEval(env, e) :: k => e match {
        case EUndef => State(k, UndefV :: s, h, mem)
        case ENum(n) => State(k, NumV(n) :: s, h, mem)
        case EBool(b) => State(k, BoolV(b) :: s, h, mem)
        case EAdd(l, r) => State(IEval(env, l) :: IEval(env, r) :: IAdd :: k, s, h, mem)
        case EMul(l, r) => State(IEval(env, l) :: IEval(env, r) :: IMul :: k, s, h, mem)
        case EDiv(l, r) => State(IEval(env, l) :: IEval(env, r) :: IDiv :: k, s, h, mem) 
        case EMod(l, r) => State(IEval(env, l) :: IEval(env, r) :: IMod :: k, s, h, mem)
        case EEq(l, r) => State(IEval(env, l) :: IEval(env, r) :: IEq :: k, s, h, mem)
        case ELt(l, r) => State(IEval(env, l) :: IEval(env, r) :: ILt :: k, s, h, mem)
        case EId(x) => State(k, mem(lookup(env, x)) :: s, h, mem)
        case EVar(x, i, b) => State(IEval(env, i) :: IDef(List(x), env, b) :: k, s, h, mem)
        case EAssign(x, e) => State(IEval(env, e) :: IWrite(lookup(env, x)) :: k, s, h, mem)
        case ESeq(l, r) => State(IEval(env, l) :: IPop :: IEval(env, r) :: k, s, h, mem)
        case EIf(c, t, e) => State(IEval(env, c) :: IJmpIf(KValue(IEval(env, t) :: k, s, h)) :: IEval(env, e) :: k, s, h, mem)
        case EWhile(c, b) => {
          val psi_continue = KValue(IPop :: IEval(env, EWhile(c, b)) :: k, s, h)
          val psi_break = KValue(k, s, h)
          val h_body = h + (Continue -> psi_continue) + (Break -> psi_break)
          val psi_body = KValue(IEval(env, b) :: IJmp(Continue) :: Nil , s, h_body)
          State(IEval(env, c) :: IJmpIf(psi_body) :: k, UndefV :: s, h, mem)
        }
        case EBreak => State(IJmp(Break) :: Nil, UndefV :: s, h, mem)
        case EContinue => State(IJmp(Continue) :: Nil, UndefV :: s, h, mem)
        case EFun(ps, b) => State(k, CloV(ps, b, env) :: s, h, mem)
        case EApp(f, es) => State(IEval(env, f) :: es.map(e => IEval(env, e)) ::: ICall(es.length) :: k, s, h, mem)
        case EReturn(e) => State(IEval(env, e) :: IReturn :: k, s, h, mem)
        case ETry(b, x, c) => {
          val h_body = h + (Throw -> KValue(IDef(List(x), env, c) :: k, s, h)) + (Finally -> KValue(k, s, h))
          State(IEval(env, b) :: IJmp(Finally) :: Nil, s, h_body, mem)
        }
        case EThrow(e) => State(IEval(env, e) :: IJmp(Throw) :: Nil, s, h, mem)
        case EGen(ps, b) => State(k, GenV(ps, b, env) :: s, h, mem)
        case EIterNext(i, a) => {
          if a == None then
            State(IEval(env, i) :: IEval(env, EUndef) :: INext :: k, s, h, mem)
            else
            State(IEval(env, i) :: IEval(env, a.get) :: INext :: k, s, h, mem)
        }
        case EYield(e) => State(IEval(env, e) :: IYield :: Nil, BoolV(false) :: ContV(KValue(k, s, h)) :: s, h, mem)
        case EValueField(r) => State(IEval(env, r) :: IValueField :: k, s, h, mem)
        case EDoneField(r) => State(IEval(env, r) :: IDoneField :: k, s, h, mem)
        case _ => error("cannot evaluate")
      }
      case IAdd :: k => s match {
        case NumV(rn) :: NumV(ln) :: s => State(k, NumV(ln + rn) :: s, h, mem)
        case _ => error("invalid operation")
      }
      case IMul :: k => s match {
        case NumV(rn) :: NumV(ln) :: s => State(k, NumV(ln * rn) :: s, h, mem)
        case _ => error("invalid operation")
      }
      case IDiv :: k => s match {
        case NumV(rn) :: NumV(ln) :: s => 
          if rn != 0 then State(k, NumV(ln / rn) :: s, h, mem) else error("division by zero")
        case _ => error("invalid operation")
      }
      case IMod :: k => s match {
        case NumV(rn) :: NumV(ln) :: s => 
          if rn != 0 then State(k, NumV(ln % rn) :: s, h, mem) else error("division by zero")
        case _ => error("invalid operation")
      }
      case IEq :: k => s match {
        case v2 :: v1 :: s => State(k, BoolV(eq(v1, v2)) :: s, h, mem)
        case _ => error("not enough values")
      }
      case ILt :: k => s match {
        case NumV(rn) :: NumV(ln) :: s => State(k, BoolV(ln < rn) :: s, h, mem)
        case _ => error("invalid operation")
      }
      case IDef(xs, env, b) :: k => s match {
        case s if s.length >= xs.length => {
          val as = malloc(mem, xs.length)
          val (vs, rest) = s.splitAt(xs.length)
          val newEnv = env ++ xs.zip(as)
          val newMem = as.zip(vs).foldLeft(mem) {
            case (m, (a, v)) => m + (a -> v) 
          }
          State(IEval(newEnv, b) :: k , rest, h, newMem)
        }
        case _ => error("not enough values")
      }
      case IWrite(a) :: k => s match {
        case v :: s => State(k, v :: s, h, mem + (a -> v))
        case _ => error("cannot write")
      }
      case IPop :: k => s match {
        case v :: s => State(k, s, h, mem)
        case _ => error("cannot pop")
      }
      case IJmpIf(kv) :: k => s match {
        case BoolV(true) :: s => State(kv.cont, kv.stack, kv.handler, mem)
        case BoolV(false) :: s => State(k, s, h, mem)
        case _ => error("not a boolean")
      }
      case IJmp(c) :: k => s match {
        case v :: s => lookup(h, c) match {
          case KValue(k1, s1, h1) => {
            if h.get(Yield) != None then
              State(k1, v :: s1, h1 + (Yield -> lookup(h, Yield)), mem)
            else
              State(k1, v :: s1, h1, mem)
          }
          case _ => error("cannot jump, there is no handler")
        }
        case _ => error("cannot jump")      
      }
      case ICall(n) :: k => s match {
        case s if s.length >= n => {
          val as = malloc(mem, n)
          val (vs, rest) = s.splitAt(n)
          rest match {
            case CloV(ps, b, env) :: rest => {
              val sBody = if ps.length >= n then
                Nil ++ vs.reverse ++ List.fill(ps.length - n)(UndefV)
              else
                Nil ++ vs.reverse.take(ps.length)
              val hBody = h.updated(Return, KValue(k, s, h)).removedAll(Set(Break, Continue, Yield))
              State(IDef(ps, env, EReturn(b)) :: Nil, sBody, hBody, mem )
            }
            case GenV(ps, b, env) :: rest => {
              val a = malloc(mem)
              val sBody = if ps.length >= n then
                Nil ++ vs.reverse ++ List.fill(ps.length - n)(UndefV)
              else
                Nil ++ vs.reverse.take(ps.length)
              val kBody = IPop :: IDef(ps, env, EReturn(ETry(b, "exception", EId("exception")))) :: Nil
              State(k, IterV(a) :: rest, h, mem + (a -> ContV(KValue(kBody, sBody, Map.empty))))
            }
            case _ => error("not a function or generator")
          }
        }
        case _ => error("not enough args")
      }
      case IReturn :: k => s match {
        case v :: s =>{
          if h.get(Yield) != None then
          State(IYield :: Nil, v :: BoolV(true) :: ContV(KValue(IReturn :: Nil, Nil, Map.empty)) :: s, h, mem)
          else
          State(IJmp(Return) :: Nil, v :: Nil, h, mem)
        }
        case _ => error("cannot return")
      }
      case INext :: k => s match {
        case v :: IterV(a) :: s => {
          mem.getOrElse(a, error("invalid address")) match {
            case ContV(KValue(k1, s1, h1)) => {
              val psi = KValue(k, IterV(a) :: s, h)
              val hBody = h1 + (Yield -> psi) + (Return -> psi)
              State(k1, v :: s1, hBody, mem)
            }
            case _ => error("not a ContV")
          }
        }
        case _ => error("cannot iterate next")
      }
      case IYield :: _ => s match {
        case v1 :: BoolV(b) :: v2 :: _ => {
          lookup(h, Yield) match {
            case KValue(k1, IterV(a) :: s1, h1) => {
              State(k1, ResultV(v1, b) :: s1, h1, mem + (a -> v2))
            }
            case _ => error("Not a ContV or Wrong stack")
         } 
        }
        case _ => error("Invalid stack ")
      }
      case IValueField :: k => s match {
        case ResultV(v, _) :: s => State(k, v :: s, h, mem)
        case _ => error("Not a ResultV")
      }
      case IDoneField :: k => s match {
        case ResultV(v, b) :: s => State(k, BoolV(b) :: s, h, mem)
        case _ => error("Not a ResultV")
      }
      case _ => s match {
        case v :: s => 
        State(Nil, v :: Nil, h, mem)
        case _ => error("there is no Result")
      }

      
    }
  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
def bodyOfSquares: String =
  """
  if (from > to) return 0 else 
  while (from <= to) {
    yield from * from;
    from += 1;
  };
  """

  // ---------------------------------------------------------------------------
  // Helper functions
  // ---------------------------------------------------------------------------
  def malloc(mem: Mem, n: Int): List[Addr] =
    val a = malloc(mem)
    (0 until n).toList.map(a + _)

  def malloc(mem: Mem): Addr = mem.keySet.maxOption.fold(0)(_ + 1)

  def lookup(env: Env, x: String): Addr =
    env.getOrElse(x, error(s"free identifier: $x"))

  def lookup(handler: Handler, x: Control): KValue =
    handler.getOrElse(x, error(s"invalid control operation: $x"))

  def eq(l: Value, r: Value): Boolean = (l, r) match
    case (UndefV, UndefV)                   => true
    case (NumV(l), NumV(r))                 => l == r
    case (BoolV(l), BoolV(r))               => l == r
    case (IterV(l), IterV(r))               => l == r
    case (ResultV(lv, ld), ResultV(rv, rd)) => eq(lv, rv) && ld == rd
    case _                                  => false
}
