package coya.quotes

import coya.models.User
import coya.models.Product

trait Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal]
}

object CoyaProcessor extends Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal] = None
}
