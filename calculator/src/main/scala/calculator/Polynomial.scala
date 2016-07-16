package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
                   c: Signal[Double]): Signal[Double] = {
    //Δ = b² - 4ac
    new Var((b() * b()) - (4 * a() * c()))
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
                       c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {
    //(-b ± √Δ) / 2a
    Var {
      val d = delta()
      if (d < 0) Set(0)
      else if (d == 0) {
        Set(-b() / 2.0 * a())
      } else {
        val a_value = a()
        val b_value = b()
        Set((-b_value - Math.sqrt(d)) / (2.0 * a_value), (-b_value + Math.sqrt(d)) / (2.0 * a_value))
      }
    }
  }
}
