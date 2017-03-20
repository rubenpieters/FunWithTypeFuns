package be.rubenpieters.fun

object p4_1_typed_sprintf extends App {

  sealed trait F[A]
  case class Lit(str: String) extends F[L]
  case class Val[Val_](parser: Parser[Val_], printer: Printer[Val_]) extends F[V[Val_]]
  case class Cmp[F1, F2](f1: F[F1], f2: F[F2]) extends F[C[F1, F2]]

  sealed trait L
  sealed trait V[Val]
  sealed trait C[F1, F2]

  type Parser[A] = String => List[(A, String)]
  type Printer[A] = A => String

  sealed trait TPrinter[A, X] {
    type Out

    def print(f: F[A], k: String => X): Out
  }

  object TPrinter {
    type Aux[F, X, Out0] = TPrinter[F, X] { type Out = Out0 }
    def apply[F, X](implicit ev: TPrinter[F, X]): Aux[F, X, ev.Out] = ev
  }

  implicit def tPrinterL[X]: TPrinter.Aux[L, X, X] = new TPrinter[L, X] {
    type Out = X

    override def print(f: F[L], k: (String) => X): X = k(f.asInstanceOf[Lit].str)
  }

  implicit def tPrinterVal[X, Val_]: TPrinter.Aux[V[Val_], X, Val_ => X] = new TPrinter[V[Val_], X] {
    type Out = Val_ => X

    override def print(f: F[V[Val_]], k: (String) => X): Val_ => X = (x: Val_) => k(f.asInstanceOf[Val[Val_]].printer(x))
  }

  sealed trait Func[O1, O2] {
    type Out
  }

  object Func {
    type Aux[O1, O2, Out0] = Func[O1, O2] { type Out = Out0 }
    def apply[O1, O2](implicit ev: Func[O1, O2]): Aux[O1, O2, ev.Out] = ev
  }



  /*implicit def tPrinterCmp[X, F1, F2, OF1, OF2]
  (implicit printerF1: TPrinter.Aux[F1, OF2, OF1]
   , printerF2: TPrinter.Aux[F2, X, OF2]
  ): TPrinter.Aux[C[F1, F2], X, OF1] = new TPrinter[C[F1, F2], X] {
    type Out = OF1

    override def print(f: F[C[F1, F2]], k: (String) => X): OF1 = {
      val cmp = f.asInstanceOf[Cmp[F1, F2]]
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(cmp.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(cmp.f1, s1 => print2(s1))

      print1
    }
  }*/

  implicit def tPrinterCmp1[X]
  (implicit printerF1: TPrinter.Aux[L, X, X]
   , printerF2: TPrinter.Aux[L, X, X]
  ): TPrinter.Aux[C[L, L], X, X] = new TPrinter[C[L, L], X] {
    type Out = X

    override def print(f: F[C[L, L]], k: (String) => X): X = {
      val cmp = f.asInstanceOf[Cmp[L, L]]
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(cmp.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(cmp.f1, s1 => print2(s1))

      print1
    }
  }

  implicit def tPrinterCmp2[X, Val1]
  (implicit printerF1: TPrinter.Aux[V[Val1], X, Val1 => X]
   , printerF2: TPrinter.Aux[L, X, X]
  ): TPrinter.Aux[C[V[Val1], L], X, Val1 => X] = new TPrinter[C[V[Val1], L], X] {
    type Out = Val1 => X

    override def print(f: F[C[V[Val1], L]], k: (String) => X): Val1 => X = {
      val cmp = f.asInstanceOf[Cmp[V[Val1], L]]
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(cmp.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(cmp.f1, s1 => print2(s1))

      print1
    }
  }

  implicit def tPrinterCmp3[X, Val1]
  (implicit printerF1: TPrinter.Aux[L, Val1 => X, Val1 => X]
   , printerF2: TPrinter.Aux[V[Val1], X, Val1 => X]
  ): TPrinter.Aux[C[L, V[Val1]], X, Val1 => X] = new TPrinter[C[L, V[Val1]], X] {
    type Out = Val1 => X

    override def print(f: F[C[L, V[Val1]]], k: (String) => X): Val1 => X = {
      val cmp = f.asInstanceOf[Cmp[L, V[Val1]]]
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(cmp.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(cmp.f1, s1 => print2(s1))

      print1
    }
  }

  implicit def tPrinterCmp4[X, Val1, Val2]
  (implicit printerF1: TPrinter.Aux[V[Val1], Val2 => X, Val1 => Val2 => X]
   , printerF2: TPrinter.Aux[V[Val2], X, Val2 => X]
  ): TPrinter.Aux[C[V[Val1], V[Val2]], X, Val1 => Val2 => X] = new TPrinter[C[V[Val1], V[Val2]], X] {
    type Out = Val1 => Val2 => X

    override def print(f: F[C[V[Val1], V[Val2]]], k: (String) => X): Val1 => Val2 => X = {
      val cmp = f.asInstanceOf[Cmp[V[Val1], V[Val2]]]
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(cmp.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(cmp.f1, s1 => print2(s1))

      print1
    }
  }

  implicit def tPrinterCmp5[X, Val1, F1, F2, O2]
  (implicit printerF1: TPrinter.Aux[V[Val1], O2, Val1 => O2]
   , printerF2: TPrinter.Aux[C[F1, F2], X, O2]
  ): TPrinter.Aux[C[V[Val1], C[F1, F2]], X, Val1 => O2] = new TPrinter[C[V[Val1], C[F1, F2]], X] {
    type Out = Val1 => O2

    override def print(f: F[C[V[Val1], C[F1, F2]]], k: (String) => X): Val1 => O2 = {
      val cmp = f.asInstanceOf[Cmp[V[Val1], C[F1, F2]]]
      def endStr(s1: String, s2: String): X = k(s1 + s2)
      def print2(s1: String) = printerF2.print(cmp.f2, s2 => endStr(s1, s2))
      def print1 = printerF1.print(cmp.f1, s1 => print2(s1))

      print1
    }
  }

  val f_ld = Lit("day")
  val f_lds: F[C[L, L]] = Cmp(Lit("day"), Lit("s"))
  val val_int = Val[Int](null, _.toString)
  val f_dn: F[C[L, V[Int]]] = Cmp(Lit("day "), val_int)
  val f_nds: F[C[V[Int], C[L, L]]] = Cmp(val_int, Cmp(Lit(" day"), Lit("s")))

  implicitly[TPrinter.Aux[L, String, String]]
  implicitly[TPrinter.Aux[L, Int => String, Int => String]]
  implicitly[TPrinter.Aux[V[Int], String, Int => String]]
  println(TPrinter[C[L, L], String].print(f_lds, identity))
  //println(TPrinter[C[L, V[Int]], String].print(f_dn, identity)(5))
  println(f_ld.sprintf)
  println(val_int.sprintf.apply(5))
  //println(f_nds.sprintf.apply(5))
  //println(f_lds.sprintf)

  implicit class printerL[A](f: F[A]) {
    def print[X, O](k: String => X)(implicit tp: TPrinter.Aux[A, X, O]): O = tp.print(f, k)
    def sprintf[O](implicit tp: TPrinter.Aux[A, String, O]): O = tp.print(f, identity)
  }

}
