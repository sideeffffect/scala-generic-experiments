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

  implicit val eq: Eq[Color] = (x: Color, y: Color) =>
    GEq.eq[Color, Color.GenericRep](x, y)(
      generic,
      GEq.sum(GEq.con(GEq.unit), GEq.sum(GEq.con(GEq.unit), GEq.con(GEq.unit)))
    )

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

object Main {
  def main(args: Array[String]): Unit = {
    println("hello")
    println(Tree.Leaf(1) === Tree.Branch(Tree.Leaf(2), Tree.Leaf(3)))
    println((Color.Green: Color) === Color.Green)
    println((Color.Green: Color) === Color.Blue)
    println(Color.`enum`)
    println(Tree.conName(Tree.Leaf(1)))
    println(Tree.conName(Tree.Branch(Tree.Leaf(2), Tree.Leaf(3))))
    println(Color.conName(Color.Green))
  }
}
