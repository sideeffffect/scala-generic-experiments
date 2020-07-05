package generic

import cats.Eq
import cats.implicits._

import scala.reflect.ClassTag

sealed trait Tree[A] extends Product with Serializable

object Tree {

  final case class Leaf[A](x: A) extends Tree[A]

  final object Leaf {
    def apply[A](x: A): Tree[A] = new Leaf(x)
  }

  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  final object Branch {
    def apply[A](l: Tree[A], r: Tree[A]): Tree[A] = new Branch(l, r)
  }

  implicit def generic[A]: Generic[Tree[A]] {
    type Rep = Either[
      Generic.Con[Generic.Wrap[A]],
      Generic.Con[(Generic.Wrap[Tree[A]], Generic.Wrap[Tree[A]])]
    ]
  } =
    new Generic[Tree[A]] {
      override type Rep = Either[
        Generic.Con[Generic.Wrap[A]],
        Generic.Con[(Generic.Wrap[Tree[A]], Generic.Wrap[Tree[A]])]
      ]

      override def from(a: Tree[A]): Rep =
        a match {
          case Leaf(x) => Left(Generic.Con("Leaf", Generic.Wrap(x)))
          case Branch(l, r) =>
            Right(Generic.Con("Branch", (Generic.Wrap(l), Generic.Wrap(r))))
        }

      override def to(rep: Rep): Tree[A] =
        rep match {
          case Left(Generic.Con(_, Generic.Wrap(value))) =>
            Leaf(value)
          case Right(Generic.Con(_, (Generic.Wrap(l), Generic.Wrap(r)))) =>
            Branch(l, r)
        }
    }

  implicit def eq[A](implicit eqA: Eq[A]): Eq[Tree[A]] = {
    (x: Tree[A], y: Tree[A]) =>
      GEq.eq(x, y)
    //(generic)(GEq.either(GEq.wrap(eqA), GEq.pair(GEq.wrap(eq(eqA)), GEq.wrap(eq(eqA)))))
  }

  def conName[A](x: Tree[A]): String =
    GConName.conName(x)
  // (generic, GConName.either(GConName.con, GConName.con))
}

trait Generic[A] {
  type Rep

  def from(a: A): Rep

  def to(rep: Rep): A
}

object Generic {

  final case class Wrap[A](unwrap: A) extends AnyVal

  final case class Con[A](name: String, value: A)

}

trait GEq[A] {
  def geq(x: A, y: A): Boolean
}

object GEq {
  def eq[A, R](
      x: A,
      y: A
  )(implicit generic: Generic[A] { type Rep = R }, geq: GEq[R]): Boolean = {
    geq.geq(generic.from(x), generic.from(y))

  }

  implicit def either[A, B](implicit
      geqA: GEq[A],
      geqB: GEq[B]
  ): GEq[Either[A, B]] =
    (x: Either[A, B], y: Either[A, B]) =>
      (x, y) match {
        case (Left(x), Left(y))   => geqA.geq(x, y)
        case (Right(x), Right(y)) => geqB.geq(x, y)
        case _                    => false
      }

  implicit def pair[A, B](implicit geqA: GEq[A], geqB: GEq[B]): GEq[(A, B)] =
    (x: (A, B), y: (A, B)) =>
      (x, y) match {
        case ((x1, x2), (y1, y2)) => geqA.geq(x1, y1) && geqB.geq(x2, y2)
      }

  implicit def wrap[A](implicit eqA: Eq[A]): GEq[Generic.Wrap[A]] =
    (x: Generic.Wrap[A], y: Generic.Wrap[A]) => eqA.eqv(x.unwrap, y.unwrap)

  implicit def con[A](implicit geqA: GEq[A]): GEq[Generic.Con[A]] =
    (x: Generic.Con[A], y: Generic.Con[A]) => geqA.geq(x.value, y.value)
}

sealed trait Color extends Product with Serializable

object Color {

  final case object Red extends Color

  final case object Green extends Color

  final case object Blue extends Color

  implicit val generic: Generic[Color] {
    type Rep =
      Either[Generic.Con[Unit], Either[Generic.Con[Unit], Generic.Con[Unit]]]
  } = new Generic[Color] {
    override type Rep =
      Either[Generic.Con[Unit], Either[Generic.Con[Unit], Generic.Con[Unit]]]

    override def from(a: Color): Rep =
      a match {
        case Red   => Left(Generic.Con("Red", ()))
        case Green => Right(Left(Generic.Con("Green", ())))
        case Blue  => Right(Right(Generic.Con("Blue", ())))
      }

    override def to(rep: Rep): Color =
      rep match {
        case Left(Generic.Con(_, ()))         => Red
        case Right(Left(Generic.Con(_, ())))  => Green
        case Right(Right(Generic.Con(_, ()))) => Blue
      }
  }

  def enum: List[Color] = GEnum.`enum`
  // (generic, GEnum.either(GEnum.unit, GEnum.either(GEnum.unit,GEnum.unit)))

  def conName(x: Color): String =
    GConName.conName(x)
  // (generic, GConName.either(GConName.con, GConName.either(GConName.con, GConName.con)))
}

trait GEnum[A] {
  def genum: List[A]
}

object GEnum {
  def enum[A, R](implicit
      generic: Generic[A] { type Rep = R },
      genum: GEnum[R]
  ): List[A] = genum.genum.map(generic.to)

  implicit def unit[A]: GEnum[Unit] =
    new GEnum[Unit] {
      override def genum: List[Unit] = List(())
    }

  implicit def either[A, B](implicit
      genumA: GEnum[A],
      genumB: GEnum[B]
  ): GEnum[Either[A, B]] =
    new GEnum[Either[A, B]] {
      override def genum: List[Either[A, B]] =
        genumA.genum.map(Left(_)) ++ genumB.genum.map(Right(_))
    }

  implicit def con[A](implicit
      genum0: GEnum[A]
  ): GEnum[Generic.Con[A]] =
    new GEnum[Generic.Con[A]] {
      override def genum: List[Generic.Con[A]] =
        genum0.genum.map(Generic.Con("<undefined>", _))
    }
}

trait GConName[A] {
  def gconName(x: A): String
}

object GConName {
  def conName[A, R](
      x: A
  )(implicit
      generic: Generic[A] { type Rep = R },
      gconName: GConName[R]
  ): String =
    gconName.gconName(generic.from(x))

  implicit def either[A, B](implicit
      gconnameA: GConName[A],
      gconnameB: GConName[B]
  ): GConName[Either[A, B]] = {
    case Left(value)  => gconnameA.gconName(value)
    case Right(value) => gconnameB.gconName(value)
  }

  implicit def con[A]: GConName[Generic.Con[A]] = (x: Generic.Con[A]) => x.name
}

object Main {
  def main(args: Array[String]): Unit = {
    println("hello")
    println(2 === 1)
    println(Tree.Leaf(1) === Tree.Branch(Tree.Leaf(2), Tree.Leaf(3)))
    println(Color.`enum`)
    println(Tree.conName(Tree.Leaf(1)))
    println(Tree.conName(Tree.Branch(Tree.Leaf(2), Tree.Leaf(3))))
    println(Color.conName(Color.Green))
  }
}
