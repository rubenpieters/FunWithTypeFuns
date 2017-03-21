package be.rubenpieters.fun

object p4_1_typed_sprintf extends App {
  sealed trait F
  case class Lit(str: String) extends F
  case class Val[A](printer: A => String) extends F
  case class Cmp[F1 <: F, F2 <: F](f1: F1, f2: F2) extends F

  val valInt = Val[Int](_.toString)
  val valStr = Val[String](identity)
  val nameAge = Cmp(Cmp(Lit("name="), valStr), Cmp(Lit(", age="), valInt))

  sealed trait TPrinter[A <: F, X] {
    type Out

    def print(f: A, k: String => X): Out
  }

  object TPrinter {
    type Aux[A <: F, X, Out0] = TPrinter[A, X] { type Out = Out0 }
  }

  implicit def tPrinterLit[X]: TPrinter.Aux[Lit, X, X] = new TPrinter[Lit, X] {
    type Out = X

    override def print(f: Lit, k: (String) => X): X = k(f.str)
  }

  implicit def tPrinterVal[X, A]: TPrinter.Aux[Val[A], X, A => X] = new
      TPrinter[Val[A], X] {
    type Out = A => X

    override def print(f: Val[A], k: (String) => X): A => X =
      (x: A) => k(f.printer(x))
  }

  implicit def tPrinterCmp[X, F1 <: F, F2 <: F, OF1, OF2]
  (implicit printerF1: TPrinter.Aux[F1, OF2, OF1]
   , printerF2: TPrinter.Aux[F2, X, OF2]
  ): TPrinter.Aux[Cmp[F1, F2], X, OF1] = new TPrinter[Cmp[F1, F2], X] {
    type Out = OF1

    override def print(f: Cmp[F1, F2], k: (String) => X): OF1 = {
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(f.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(f.f1, s1 => print2(s1))

      print1
    }
  }

  println(implicitly[TPrinter.Aux[Cmp[Cmp[Lit, Val[String]], Cmp[Lit,
    Val[Int]]], String, String => Int => String]].print(nameAge, identity)("ruben")(23))

  implicit class printerOps[A <: F](f: A) {
    def print[X, O](k: String => X)(implicit tp: TPrinter.Aux[A, X, O]):
    O = tp.print(f, k)
    def sprintf[O](implicit tp: TPrinter.Aux[A, String, O]): O =
      tp.print(f, identity)
  }

  println(nameAge.sprintf[String => Int => String].apply("ruben")(23))
}
