package be.rubenpieters.fun

case object Done
case class In[A, B](f: A => B)
case class Out[A, B](a: A, b: () => B)

object p3_4_session_types_and_their_duality extends App {
  type Stop = Done.type

  trait Session[A] {
    type Dual
    def run(a: A, dual: Dual): Unit
  }

  object Session {
    type Aux[A, B] = Session[A] { type Dual = B }
    def apply[A](implicit ev: Session[A]): Aux[A, ev.Dual] = ev
  }

  implicit def sessionIn[A, B, R](implicit sb: Session.Aux[B, R]): Session.Aux[In[A, B], Out[A, R]] =
    new Session[In[A, B]] {
      type Dual = Out[A, R]
      override def run(in: In[A, B], out: Out[A, R]) = {
        val b: B = in.f(out.a)
        val c: R = out.b()
        sb.run(b, c)
      }
    }

  implicit def sessionOut[A, B, R](implicit sb: Session.Aux[B, R]): Session.Aux[Out[A, B], In[A, R]] =
    new Session[Out[A, B]] {
      type Dual = In[A, R]
      override def run(out: Out[A, B], in: In[A, R]) = {
        val b: R = in.f(out.a)
        val c: B = out.b()
        sb.run(c, b)
      }
    }

  implicit val sessionStop: Session.Aux[Stop, Stop] = new Session[Stop] {
    type Dual = Stop
    override def run(a: Stop, b: Stop) = ()
  }

  type Server = In[Int, In[Int, Out[Int, Stop]]]

  val add_server: Server = In((x: Int) => In((y: Int) => {
    println("Thinking")
    Out(x + y, () => Done)
  }))

  type Client = Out[Int, Out[Int, In[Int, Stop]]]

  val add_client: Client = Out(3, () => Out(4, () => {
    println("Waiting")
    In((z: Int) => {
      println(z)
      Done
    })
  }))

  println("---")
  Session[Server].run(add_server, add_client)
  println("---")
  Session[Client].run(add_client, add_server)

  // could also define implicit ops for some convenience
  implicit class SessionOps[A](a: A){
    def runSess[B](b: B)(implicit sa: Session.Aux[A, B]) = sa.run(a, b)
  }

  println("---")
  add_server.runSess(add_client)
  println("---")
  add_client.runSess(add_server)

  type Neg = In[Int, Out[Int, Stop]]

  val neg_server: Neg = In((x: Int) => {
    println("thinking")
    Out(-x, () => Done)
  })

  // cannot compose two processes with non-dual protocols :
  // Session[Server].run(add_server, neg_server)
  // add_server.runSess(neg_server)
  // neg_server.runSess(add_server)
}
