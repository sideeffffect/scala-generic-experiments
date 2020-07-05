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

  type GenericRep[A] = Generic.Representation.Sum[
    Generic.Representation.Con[Generic.Representation.Atom[A]],
    Generic.Representation.Con[
      Generic.Representation.Product[
        Generic.Representation.Atom[Tree[A]],
        Generic.Representation.Atom[Tree[A]]
      ]
    ]
  ]
  implicit def generic[A]: Generic[Tree[A]] { type Rep = GenericRep[A] } =
    new Generic[Tree[A]] {
      override type Rep = GenericRep[A]

      override def from(a: Tree[A]): Rep =
        a match {
          case Leaf(x) =>
            Generic.Representation.Sum.Left(
              Generic.Representation.Con("Leaf", Generic.Representation.Atom(x))
            )
          case Branch(l, r) =>
            Generic.Representation.Sum.Right(
              Generic.Representation.Con(
                "Branch",
                Generic.Representation.Product(
                  Generic.Representation.Atom(l),
                  Generic.Representation.Atom(r)
                )
              )
            )
        }

      override def to(rep: Rep): Tree[A] =
        rep match {
          case Generic.Representation.Sum.Left(
                Generic.Representation
                  .Con(_, Generic.Representation.Atom(value))
              ) =>
            Leaf(value)
          case Generic.Representation.Sum.Right(
                Generic.Representation.Con(
                  _,
                  Generic.Representation.Product(
                    Generic.Representation.Atom(l),
                    Generic.Representation.Atom(r)
                  )
                )
              ) =>
            Branch(l, r)
        }
    }

  implicit def eq[A](implicit eqA: Eq[A]): Eq[Tree[A]] = {
    (x: Tree[A], y: Tree[A]) =>
      GEq.eq[Tree[A], Tree.GenericRep[A]](x, y)
    // (generic, GEq.either(GEq.con(GEq.wrap(eqA)), GEq.con(GEq.pair(GEq.wrap(eq(eqA)), GEq.wrap(eq(eqA))))))
  }

  def conName[A](x: Tree[A]): String =
    GConName.conName(x)
  // (generic, GConName.either(GConName.con, GConName.con))
}

trait Generic[A] {
  type Rep <: Generic.Representation

  def from(a: A): Rep

  def to(rep: Rep): A
}

object Generic {

  sealed trait Representation extends Product with Serializable

  final object Representation {

    sealed trait Sum[+A <: Representation, +B <: Representation]
        extends Representation
    final object Sum {
      final case class Left[A <: Representation](l: A) extends Sum[A, Nothing]
      final case class Right[B <: Representation](r: B) extends Sum[Nothing, B]
    }

    final case class Product[+A <: Representation, +B <: Representation](
        _1: A,
        _2: B
    ) extends Representation

    final case object Unit extends Representation

    final case class Atom[+A](unwrap: A) extends Representation

    final case class Con[+A <: Representation](name: String, value: A)
        extends Representation
  }

}

trait GEq[A <: Generic.Representation] {
  def geq(x: A, y: A): Boolean
}

object GEq {
  def eq[A, R <: Generic.Representation](
      x: A,
      y: A
  )(implicit generic: Generic[A] { type Rep = R }, geq: GEq[R]): Boolean = {
    geq.geq(generic.from(x), generic.from(y))

  }

  implicit def either[A <: Generic.Representation, B <: Generic.Representation](
      implicit
      geqA: GEq[A],
      geqB: GEq[B]
  ): GEq[Generic.Representation.Sum[A, B]] =
    (
        x: Generic.Representation.Sum[A, B],
        y: Generic.Representation.Sum[A, B]
    ) =>
      (x, y) match {
        case (
              Generic.Representation.Sum.Left(x),
              Generic.Representation.Sum.Left(y)
            ) =>
          geqA.geq(x, y)
        case (
              Generic.Representation.Sum.Right(x),
              Generic.Representation.Sum.Right(y)
            ) =>
          geqB.geq(x, y)
        case _ => false
      }

  implicit def pair[A <: Generic.Representation, B <: Generic.Representation](
      implicit
      geqA: GEq[A],
      geqB: GEq[B]
  ): GEq[Generic.Representation.Product[A, B]] =
    (
        x: Generic.Representation.Product[A, B],
        y: Generic.Representation.Product[A, B]
    ) =>
      (x, y) match {
        case (
              Generic.Representation.Product(x1, x2),
              Generic.Representation.Product(y1, y2)
            ) =>
          geqA.geq(x1, y1) && geqB.geq(x2, y2)
      }

  implicit def wrap[A](implicit
      eqA: Eq[A]
  ): GEq[Generic.Representation.Atom[A]] =
    (x: Generic.Representation.Atom[A], y: Generic.Representation.Atom[A]) =>
      eqA.eqv(x.unwrap, y.unwrap)

  implicit def con[A <: Generic.Representation](implicit
      geqA: GEq[A]
  ): GEq[Generic.Representation.Con[A]] =
    (x: Generic.Representation.Con[A], y: Generic.Representation.Con[A]) =>
      geqA.geq(x.value, y.value)
}

sealed trait Color extends Product with Serializable

object Color {

  final case object Red extends Color

  final case object Green extends Color

  final case object Blue extends Color

  type GenericRep =
    Generic.Representation.Sum[
      Generic.Representation.Con[Generic.Representation.Unit.type],
      Generic.Representation.Sum[Generic.Representation.Con[
        Generic.Representation.Unit.type
      ], Generic.Representation.Con[Generic.Representation.Unit.type]]
    ]
  implicit val generic: Generic[Color] { type Rep = GenericRep } =
    new Generic[Color] {
      override type Rep = GenericRep

      override def from(a: Color): Rep =
        a match {
          case Red =>
            Generic.Representation.Sum.Left(
              Generic.Representation.Con("Red", Generic.Representation.Unit)
            )
          case Green =>
            Generic.Representation.Sum.Right(
              Generic.Representation.Sum.Left(
                Generic.Representation.Con("Green", Generic.Representation.Unit)
              )
            )
          case Blue =>
            Generic.Representation.Sum.Right(
              Generic.Representation.Sum.Right(
                Generic.Representation.Con("Blue", Generic.Representation.Unit)
              )
            )
        }

      override def to(rep: Rep): Color =
        rep match {
          case Generic.Representation.Sum
                .Left(
                  Generic.Representation.Con(_, Generic.Representation.Unit)
                ) =>
            Red
          case Generic.Representation.Sum.Right(
                Generic.Representation.Sum
                  .Left(
                    Generic.Representation.Con(_, Generic.Representation.Unit)
                  )
              ) =>
            Green
          case Generic.Representation.Sum.Right(
                Generic.Representation.Sum
                  .Right(
                    Generic.Representation.Con(_, Generic.Representation.Unit)
                  )
              ) =>
            Blue
        }
    }

  def enum: List[Color] = GEnum.`enum`
  // (generic, GEnum.either(GEnum.unit, GEnum.either(GEnum.unit,GEnum.unit)))

  def conName(x: Color): String =
    GConName.conName(x)
  // (generic, GConName.either(GConName.con, GConName.either(GConName.con, GConName.con)))
}

trait GEnum[A <: Generic.Representation] {
  def genum: List[A]
}

object GEnum {
  def enum[A, R <: Generic.Representation](implicit
      generic: Generic[A] { type Rep = R },
      genum: GEnum[R]
  ): List[A] = genum.genum.map(generic.to)

  implicit def unit: GEnum[Generic.Representation.Unit.type] =
    new GEnum[Generic.Representation.Unit.type] {
      override def genum: List[Generic.Representation.Unit.type] =
        List(Generic.Representation.Unit)
    }

  implicit def either[A <: Generic.Representation, B <: Generic.Representation](
      implicit
      genumA: GEnum[A],
      genumB: GEnum[B]
  ): GEnum[Generic.Representation.Sum[A, B]] =
    new GEnum[Generic.Representation.Sum[A, B]] {
      override def genum: List[Generic.Representation.Sum[A, B]] =
        genumA.genum.map(Generic.Representation.Sum.Left(_)) ++ genumB.genum
          .map(Generic.Representation.Sum.Right(_))
    }

  implicit def con[A <: Generic.Representation](implicit
      genum0: GEnum[A]
  ): GEnum[Generic.Representation.Con[A]] =
    new GEnum[Generic.Representation.Con[A]] {
      override def genum: List[Generic.Representation.Con[A]] =
        genum0.genum.map(Generic.Representation.Con("<undefined>", _))
    }
}

trait GConName[A <: Generic.Representation] {
  def gconName(x: A): String
}

object GConName {
  def conName[A, R <: Generic.Representation](
      x: A
  )(implicit
      generic: Generic[A] { type Rep = R },
      gconName: GConName[R]
  ): String =
    gconName.gconName(generic.from(x))

  implicit def either[A <: Generic.Representation, B <: Generic.Representation](
      implicit
      gconnameA: GConName[A],
      gconnameB: GConName[B]
  ): GConName[Generic.Representation.Sum[A, B]] = {
    case Generic.Representation.Sum.Left(value)  => gconnameA.gconName(value)
    case Generic.Representation.Sum.Right(value) => gconnameB.gconName(value)
  }

  implicit def con[A <: Generic.Representation]
      : GConName[Generic.Representation.Con[A]] =
    (x: Generic.Representation.Con[A]) => x.name
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
