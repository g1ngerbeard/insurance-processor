package insurance.quotes

import insurance.models._

trait Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal]
}

object DefaultProcessor extends Processor {

  def priceFor(user: User, products: Seq[Product]): Option[BigDecimal] =
    products
      .map(calculateSubtotal(user, _))
      .reduce[Option[BigDecimal]]{
        case (resOpt, subtotalOpt) => for {
          res      <- resOpt
          subtotal <- subtotalOpt
        } yield res + subtotal
      }
      .map(_.setScale(0, BigDecimal.RoundingMode.FLOOR))

  private val userRiskToSurcharge: Vector[(Int, BigDecimal)] = Vector(
    20 -> 0.3,
    200 -> 1,
    500 -> 3
  )

  private val houseRiskSurcharge: Vector[(Int, BigDecimal)] = Vector(
    100 -> 0.7,
    299 -> 1,
    501 -> 2.5
  )

  private def calculateUserSurcharge(user: User): Option[BigDecimal] =
    userRiskToSurcharge
      .collectFirst {
        case (limit, surcharge) if user.risk <= limit => surcharge
      }

  private def calculateSubtotal(u: User, p: Product): Option[BigDecimal] =
    for {
      userSurcharge    <- calculateUserSurcharge(u)
      productSurcharge <- calculateProductSurcharge(u, p)
      subtotal         = p.value * userSurcharge * productSurcharge
      if validateSubtotal(u, p, subtotal)
    } yield subtotal

  private def calculateProductSurcharge(u: User, p: Product): Option[BigDecimal] =
    p match {
      case h: House   => houseSurcharge(h).map(_ * 0.03)
      case b: Banana  => bananaSurcharge(b, u).map(_ * 1.15)
      case b: Bicycle => bicycleSurcharge(b).map(_ * 0.10)
      case _          => None
    }

  private def validateSubtotal(u: User, p: Product, subtotal: BigDecimal): Boolean = {
    p match {
      case _: Bicycle => subtotal <= 100 || u.risk <= 150
      case _          => true
    }
  }

  private def houseSurcharge(house: House): Option[BigDecimal] = {
    if (house.value > 10000000) {
      Some(1.15)
    } else if (30 > house.size || house.size > 1000) {
      None
    } else {
      houseRiskSurcharge
        .collectFirst {
          case (limit, surcharge) if house.address.locationRisk <= limit => surcharge
        }
    }
  }

  private def bananaSurcharge(banana: Banana, user: User): Option[BigDecimal] =
    if ((3 <= banana.blackSpots && banana.blackSpots <= 12) || user.risk > 200) {
      None
    } else {
      Some(1)
    }

  private def bicycleSurcharge(bike: Bicycle): Option[Double] = Some(bike.gears * 0.08)

}
