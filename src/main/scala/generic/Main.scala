package generic

import cats.Eq
import cats.implicits._

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

  type GenericRep[A] = Representation.Sum[
    Representation.Con[Representation.Atom[A]],
    Representation.Con[Representation.Product[Representation.Atom[Tree[A]], Representation.Atom[Tree[A]]]]
  ]

  implicit def generic[A]: Generic[Tree[A]] { type Rep = GenericRep[A] } =
    new Generic[Tree[A]] {
      override type Rep = GenericRep[A]

      override def from(a: Tree[A]): Rep =
        a match {
          case Leaf(x) =>
            Representation.Sum.Left(Representation.Con("Leaf", Representation.Atom(x)))
          case Branch(l, r) =>
            Representation.Sum.Right(
              Representation.Con(
                "Branch",
                Representation.Product(Representation.Atom(l), Representation.Atom(r))
              )
            )
        }

      override def to(rep: Rep): Tree[A] =
        rep match {
          case Representation.Sum.Left(Representation.Con(_, Representation.Atom(value))) =>
            Leaf(value)
          case Representation.Sum.Right(
                Representation.Con(_, Representation.Product(Representation.Atom(l), Representation.Atom(r)))
              ) =>
            Branch(l, r)
        }
    }

  implicit def eq[A](implicit eqA: Eq[A]): Eq[Tree[A]] = { (x: Tree[A], y: Tree[A]) =>
    GEq.eq[Tree[A], Tree.GenericRep[A]](x, y)(
      generic,
      GEq.sum(
        GEq.con(GEq.atom(eqA)),
        GEq.con(GEq.product(GEq.atom(eq(eqA)), GEq.atom(eq(eqA))))
      )
    )
  }

  def conName[A](x: Tree[A]): String =
    GConName.conName[Tree[A], Tree.GenericRep[A]](x)(
      generic,
      GConName.sum(GConName.con, GConName.con)
    )
}

trait Generic[A] {
  type Rep <: Representation

  def from(a: A): Rep

  def to(rep: Rep): A
}

sealed trait Representation extends Product with Serializable

object Representation {

  sealed trait Sum[+A <: Representation, +B <: Representation] extends Representation

  final object Sum {

    final case class Left[A <: Representation](l: A) extends Sum[A, Nothing]

    final case class Right[B <: Representation](r: B) extends Sum[Nothing, B]

  }

  final case class Product[+A <: Representation, +B <: Representation](_1: A, _2: B) extends Representation

  final case object Unit extends Representation

  final case class Atom[+A](unwrap: A) extends Representation

  final case class Con[+A <: Representation](name: String, value: A) extends Representation

}

trait GEq[A <: Representation] {
  def geq(x: A, y: A): Boolean
}

object GEq {
  def eq[A, R <: Representation](x: A, y: A)(implicit
      generic: Generic[A] { type Rep = R },
      geq: GEq[R]
  ): Boolean = {
    geq.geq(generic.from(x), generic.from(y))

  }

  implicit def sum[A <: Representation, B <: Representation](implicit
      geqA: GEq[A],
      geqB: GEq[B]
  ): GEq[Representation.Sum[A, B]] =
    (x: Representation.Sum[A, B], y: Representation.Sum[A, B]) =>
      (x, y) match {
        case (Representation.Sum.Left(x), Representation.Sum.Left(y)) =>
          geqA.geq(x, y)
        case (Representation.Sum.Right(x), Representation.Sum.Right(y)) =>
          geqB.geq(x, y)
        case _ => false
      }

  implicit def product[A <: Representation, B <: Representation](implicit
      geqA: GEq[A],
      geqB: GEq[B]
  ): GEq[Representation.Product[A, B]] =
    (x: Representation.Product[A, B], y: Representation.Product[A, B]) =>
      (x, y) match {
        case (Representation.Product(x1, x2), Representation.Product(y1, y2)) =>
          geqA.geq(x1, y1) && geqB.geq(x2, y2)
      }

  implicit def atom[A](implicit eqA: Eq[A]): GEq[Representation.Atom[A]] =
    (x: Representation.Atom[A], y: Representation.Atom[A]) => eqA.eqv(x.unwrap, y.unwrap)

  implicit def con[A <: Representation](implicit
      geqA: GEq[A]
  ): GEq[Representation.Con[A]] =
    (x: Representation.Con[A], y: Representation.Con[A]) => geqA.geq(x.value, y.value)
}

sealed trait Color extends Product with Serializable

object Color {

  final case object Red extends Color

  final case object Green extends Color

  final case object Blue extends Color

  type GenericRep =
    Representation.Sum[Representation.Con[Representation.Unit.type], Representation.Sum[Representation.Con[
      Representation.Unit.type
    ], Representation.Con[Representation.Unit.type]]]
  implicit val generic: Generic[Color] { type Rep = GenericRep } =
    new Generic[Color] {
      override type Rep = GenericRep

      override def from(a: Color): Rep =
        a match {
          case Red =>
            Representation.Sum.Left(Representation.Con("Red", Representation.Unit))
          case Green =>
            Representation.Sum.Right(
              Representation.Sum.Left(Representation.Con("Green", Representation.Unit))
            )
          case Blue =>
            Representation.Sum.Right(
              Representation.Sum.Right(Representation.Con("Blue", Representation.Unit))
            )
        }

      override def to(rep: Rep): Color =
        rep match {
          case Representation.Sum.Left(Representation.Con(_, Representation.Unit)) =>
            Red
          case Representation.Sum.Right(
                Representation.Sum.Left(Representation.Con(_, Representation.Unit))
              ) =>
            Green
          case Representation.Sum.Right(
                Representation.Sum.Right(Representation.Con(_, Representation.Unit))
              ) =>
            Blue
        }
    }

  def enum: List[Color] =
    GEnum.`enum`[Color, Color.GenericRep](
      generic,
      GEnum.sum(
        GEnum.con(GEnum.unit),
        GEnum.sum(GEnum.con(GEnum.unit), GEnum.con(GEnum.unit))
      )
    )

  def conName(x: Color): String =
    GConName.conName[Color, Color.GenericRep](x)(
      generic,
      GConName.sum(GConName.con, GConName.sum(GConName.con, GConName.con))
    )
}

trait GEnum[A <: Representation] {
  def genum: List[A]
}

object GEnum {
  def enum[A, R <: Representation](implicit generic: Generic[A] { type Rep = R }, genum: GEnum[R]): List[A] =
    genum.genum.map(generic.to)

  implicit def unit: GEnum[Representation.Unit.type] =
    new GEnum[Representation.Unit.type] {
      override def genum: List[Representation.Unit.type] =
        List(Representation.Unit)
    }

  implicit def sum[A <: Representation, B <: Representation](implicit
      genumA: GEnum[A],
      genumB: GEnum[B]
  ): GEnum[Representation.Sum[A, B]] =
    new GEnum[Representation.Sum[A, B]] {
      override def genum: List[Representation.Sum[A, B]] =
        genumA.genum.map(Representation.Sum.Left(_)) ++ genumB.genum.map(Representation.Sum.Right(_))
    }

  implicit def con[A <: Representation](implicit genum0: GEnum[A]): GEnum[Representation.Con[A]] =
    new GEnum[Representation.Con[A]] {
      override def genum: List[Representation.Con[A]] =
        genum0.genum.map(Representation.Con("<undefined>", _))
    }
}

trait GConName[A <: Representation] {
  def gconName(x: A): String
}

object GConName {
  def conName[A, R <: Representation](
      x: A
  )(implicit generic: Generic[A] { type Rep = R }, gconName: GConName[R]): String =
    gconName.gconName(generic.from(x))

  implicit def sum[A <: Representation, B <: Representation](implicit
      gconnameA: GConName[A],
      gconnameB: GConName[B]
  ): GConName[Representation.Sum[A, B]] = {
    case Representation.Sum.Left(value)  => gconnameA.gconName(value)
    case Representation.Sum.Right(value) => gconnameB.gconName(value)
  }

  implicit def con[A <: Representation]: GConName[Representation.Con[A]] =
    (x: Representation.Con[A]) => x.name
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
