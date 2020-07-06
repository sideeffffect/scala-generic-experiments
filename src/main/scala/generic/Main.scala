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

  type GenericRep[A] = Representation.Sum.Case[
    Representation.Product.Factor[A, Representation.Product.Unit.type],
    Representation.Sum.Case.Constructor[
      Representation.Product.Factor[Tree[A], Representation.Product.Factor[Tree[
        A,
      ], Representation.Product.Unit.type]],
    ],
  ]
  implicit def generic[A]: Generic[Tree[A]] { type Rep = GenericRep[A] } =
    new Generic[Tree[A]] {
      override type Rep = GenericRep[A]

      override def from(a: Tree[A]): Rep =
        a match {
          case Leaf(x) =>
            Representation.Sum.Case.Constructor(
              "Leaf",
              Representation.Product.Factor("x", x, Representation.Product.Unit),
            )
          case Branch(l, r) =>
            Representation.Sum.Case.Next(
              Representation.Sum.Case.Constructor(
                "Branch",
                Representation.Product
                  .Factor("l", l, Representation.Product.Factor("r", r, Representation.Product.Unit)),
              ),
            )
        }

      override def to(rep: Rep): Tree[A] =
        rep match {
          case Representation.Sum.Case
                .Constructor(_, Representation.Product.Factor(_, value, Representation.Product.Unit)) =>
            Leaf(value)
          case Representation.Sum.Case.Next(
                Representation.Sum.Case.Constructor(
                  _,
                  Representation.Product.Factor(
                    _,
                    l,
                    Representation.Product.Factor(_, r, Representation.Product.Unit),
                  ),
                ),
              ) =>
            Branch(l, r)
        }
    }

  implicit def eq[A](implicit eqA: Eq[A]): Eq[Tree[A]] = { (x: Tree[A], y: Tree[A]) =>
    GEq.eq[Tree[A], Tree.GenericRep[A]](x, y)(
      generic,
      GEq.sumCase(
        GEq.productFactor(eqA, GEq.productUnit),
        GEq.sumCaseConstructor(GEq.productFactor(eq(eqA), GEq.productFactor(eq(eqA), GEq.productUnit))),
      ),
    )
  }

  def conName[A](x: Tree[A]): String =
    GConName.conName[Tree[A], Tree.GenericRep[A]](x)(
      generic,
      GConName.sumCase(GConName.sumCaseConstructor),
    )
}

sealed trait Color extends Product with Serializable

object Color {

  final case object Red extends Color

  final case object Green extends Color

  final case object Blue extends Color

  type GenericRep =
    Representation.Sum.Case[Representation.Product.Unit.type, Representation.Sum.Case[
      Representation.Product.Unit.type,
      Representation.Sum.Case.Constructor[Representation.Product.Unit.type],
    ]]
  implicit val generic: Generic[Color] { type Rep = GenericRep } =
    new Generic[Color] {
      override type Rep = GenericRep

      override def from(a: Color): Rep =
        a match {
          case Red =>
            Representation.Sum.Case.Constructor("Red", Representation.Product.Unit)
          case Green =>
            Representation.Sum.Case.Next(
              Representation.Sum.Case.Constructor("Green", Representation.Product.Unit),
            )
          case Blue =>
            Representation.Sum.Case.Next(
              Representation.Sum.Case.Next(
                Representation.Sum.Case.Constructor("Blue", Representation.Product.Unit),
              ),
            )
        }

      override def to(rep: Rep): Color =
        rep match {
          case Representation.Sum.Case.Constructor(_, Representation.Product.Unit) =>
            Red
          case Representation.Sum.Case.Next(
                Representation.Sum.Case.Constructor(_, Representation.Product.Unit),
              ) =>
            Green
          case Representation.Sum.Case.Next(
                Representation.Sum.Case
                  .Next(Representation.Sum.Case.Constructor(_, Representation.Product.Unit)),
              ) =>
            Blue
        }
    }

  implicit val eq: Eq[Color] = (x: Color, y: Color) =>
    GEq.eq[Color, Color.GenericRep](x, y)(
      generic,
      GEq.sumCase(GEq.productUnit, GEq.sumCase(GEq.productUnit, GEq.sumCaseConstructor(GEq.productUnit))),
    )

  def enum: List[Color] =
    GEnum.`enum`[Color, Color.GenericRep](
      generic,
      GEnum.sumCase(GEnum.sumCase(GEnum.sumCaseConstructor)),
    )

  def conName(x: Color): String =
    GConName.conName[Color, Color.GenericRep](x)(
      generic,
      GConName.sumCase(GConName.sumCase(GConName.sumCaseConstructor)),
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
