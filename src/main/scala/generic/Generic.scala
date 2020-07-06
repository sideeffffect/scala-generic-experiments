package generic

import cats.Eq

trait Generic[A] {
  type Rep <: Representation.Sum[_, _]

  def from(a: A): Rep

  def to(rep: Rep): A
}

sealed trait Representation extends Product with Serializable

//object Representation {
//
//  sealed trait Sum[+A <: Representation, +B <: Representation] extends Representation
//
//  final object Sum {
//
//    final case class Left[+A <: Representation](l: A) extends Sum[A, Nothing]
//
//    final case class Right[+B <: Representation](r: B) extends Sum[Nothing, B]
//
//  }
//
//  final case class Product[+A <: Representation, +B <: Representation](_1: A, _2: B) extends Representation
//
//  final case object Unit extends Representation
//
//  final case class Atom[+A](unwrap: A) extends Representation
//
//  final case class Con[+A <: Representation](name: String, value: A) extends Representation
//
//}

object Representation {

  sealed trait Sum[+P <: Product[_, _], +S <: Sum[_, _]] extends Representation
  final object Sum {
    final case object Void extends Sum[Nothing, Nothing]
    sealed trait Case[+P <: Product[_, _], +S <: Sum[_, _]] extends Sum[P, S]
    final object Case {
      final case class Constructor[+P <: Product[_, _]](
          constructorName: String,
          product: P
      ) extends Sum[P, Nothing]
      final case class Next[+S <: Sum[_, _]](
          sum: S
      ) extends Sum[Nothing, S]
    }
  }

  sealed trait Product[+A, +P <: Product[_, _]] extends Representation
  final object Product {
    final case object Unit extends Product[Nothing, Nothing]
    final case class Factor[+A, +P <: Product[_, _]](
        fieldName: String,
        atom: A,
        product: P
    ) extends Product[A, P]
  }

//  val x: Sum[Product.Unit.type, Sum[
//    Product.Factor[Int, Product.Factor[List[Int], Product.Unit.type]],
//    Sum.Void.type
//  ]] =
//    Sum.Case(
//      "Nil",
//      Product.Unit,
//      Sum.Case(
//        "Cons",
//        Product.Factor("head", 1, Product.Factor("tail", List.Cons(2, List.Nil), Product.Unit)),
//        Sum.Void
//      )
//    )
//
//  sealed trait List[+A] extends scala.Product with Serializable
//  final object List {
//    final case object Nil extends List[Nothing]
//    final case class Cons[A](head: A, tail: List[A]) extends List[A]
//  }

}

trait GEq[A <: Representation] {
  def geq(x: A, y: A): Boolean
}

object GEq {
  def eq[A, R <: Representation.Sum[_, _]](x: A, y: A)(implicit
      generic: Generic[A] { type Rep = R },
      geq: GEq[R]
  ): Boolean = {
    geq.geq(generic.from(x), generic.from(y))

  }

  implicit def sum[P <: Representation.Product[_, _], S <: Representation.Sum[_, S]](implicit
      geqA: GEq[P],
      geqB: GEq[S]
  ): GEq[Representation.Sum[P, S]] =
    (x: Representation.Sum[P, S], y: Representation.Sum[P, S]) =>
      (x, y) match {
        case (Representation.Sum.Case.Constructor(_, x), Representation.Sum.Case.Constructor(_, y)) =>
          geqA.geq(x, y)
        case (Representation.Sum.Case.Next(x), Representation.Sum.Case.Next(y)) =>
          geqB.geq(x, y)
        case _ => false
      }

  implicit val void: GEq[Representation.Sum.Void.type] =
    (x: Representation.Sum.Void.type, y: Representation.Sum.Void.type) => true

  implicit def product[A, P <: Representation.Product[_, P]](implicit
      eqA: Eq[A],
      geqB: GEq[P]
  ): GEq[Representation.Product[A, P]] =
    (x: Representation.Product[A, P], y: Representation.Product[A, P]) =>
      (x, y) match {
        case (Representation.Product.Factor(_, x1, x2), Representation.Product.Factor(_, y1, y2)) =>
          eqA.eqv(x1, y1) && geqB.geq(x2, y2)
        case _ => false
      }
  implicit val unit: GEq[Representation.Product.Unit.type] =
    (x: Representation.Product.Unit.type, y: Representation.Product.Unit.type) => true
}

trait GEnum[S <: Representation.Sum[Representation.Product.Unit.type, _]] {
  def genum: List[S]
}

object GEnum {
  def enum[A, R <: Representation.Sum[Representation.Product.Unit.type, _]](implicit
      generic: Generic[A] { type Rep = R },
      genum: GEnum[R]
  ): List[A] =
    genum.genum.map(generic.to)

  implicit def sum[S <: Representation.Sum[Representation.Product.Unit.type, _]](implicit
      genumS: GEnum[S]
  ): GEnum[Representation.Sum[Representation.Product.Unit.type, S]] =
    new GEnum[Representation.Sum[Representation.Product.Unit.type, S]] {
      override def genum: List[Representation.Sum[Representation.Product.Unit.type, S]] =
        List(Representation.Sum.Case.Constructor("<undefined>", Representation.Product.Unit)) ++
          genumS.genum.map(Representation.Sum.Case.Next(_))
    }

  implicit val void: GEnum[Representation.Sum.Void.type] = new GEnum[Representation.Sum.Void.type] {
    override def genum: List[Representation.Sum.Void.type] = List()
  }

}

trait GConName[S <: Representation.Sum[_, _]] {
  def gconName(x: S): String
}

object GConName {
  def conName[A, R <: Representation.Sum[_, _]](
      x: A
  )(implicit generic: Generic[A] { type Rep = R }, gconName: GConName[R]): String =
    gconName.gconName(generic.from(x))

  implicit def sum[P <: Representation.Product[_, _], S <: Representation.Sum[_, _]](implicit
      gconnameS: GConName[S]
  ): GConName[Representation.Sum[P, S]] = {
    case Representation.Sum.Case.Constructor(constructorName, _) => constructorName
    case Representation.Sum.Case.Next(sum)                       => gconnameS.gconName(sum)
  }

  implicit val void: GConName[Representation.Sum.Void.type] =
    (x: Representation.Sum.Void.type) => "<undefined>"

}
