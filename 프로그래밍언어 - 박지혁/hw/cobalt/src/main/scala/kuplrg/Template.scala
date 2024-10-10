package kuplrg

trait Template {

  // ---------------------------------------------------------------------------
  // Problem #1
  // ---------------------------------------------------------------------------
  def eval(str: String): String = interp(Expr(str), Map.empty).str

  def interp(expr: Expr, env: Env): Value

  // ---------------------------------------------------------------------------
  // Problem #2
  // ---------------------------------------------------------------------------
  def sqsumIfExpr(lists: String, pred: String): String = s"""
    val sumIf = (lists, pred) => {
      (for {
        $subExpr1
      } yield $subExpr2)
        .foldLeft(0, (x, y) => x + y)
    };
    sumIf($lists, $pred)
  """

  def subExpr1: String

  def subExpr2: String
}
