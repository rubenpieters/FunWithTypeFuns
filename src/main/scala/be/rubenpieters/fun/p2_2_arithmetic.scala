package be.rubenpieters.fun

object p2_2_arithmetic extends App {
  sealed trait Add[A, B] {
    type SumTy
    def add(a: A, b: B): SumTy
  }

  object Add {
    type Aux[A, B, S] = Add[A, B] { type SumTy = S }
    def apply[A, B](implicit ev: Add[A, B]): Aux[A, B, ev.SumTy] = ev
  }

  implicit val integerDoubleAdd: Add.Aux[Int, Double, Double] = new Add[Int, Double] {
    type SumTy = Double
    override def add(a: Int, b: Double): Double = a.toDouble + b
  }

  implicit val doubleIntegerAdd: Add.Aux[Double, Int, Double] = new Add[Double, Int] {
    type SumTy = Double
    override def add(a: Double, b: Int): Double = a + b.toDouble
  }

  val result1: Double = Add[Int, Double].add(1, 1.0)
  val result2: Double = Add[Double, Int].add(1.0, 1)

  implicit def numericAdd[A](implicit numA: Numeric[A]): Add.Aux[A, A, A] = new Add[A, A] {
    type SumTy = A
    override def add(a: A, b: A): A = numA.plus(a, b)
  }
}
