package coya.quotes

import coya.models._

trait Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal]
}

object CoyaProcessor extends Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal] = None

  private def baseProductSurcharge(product: Product): Option[BigDecimal] =
    product match {
      case _: House   => Some(BigDecimal(0.03))
      case _: Banana  =>Some(BigDecimal(1.15))
      case _: Bicycle =>Some(BigDecimal(0.10))
      case _          => None

    }
}


