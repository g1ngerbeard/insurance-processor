package coya.quotes

import coya.models._

trait Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal]
}

object CoyaProcessor extends Processor {


  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal] = None

  val userRiskToSurcharge = Vector(
    20 -> 0.3,
    200 -> 1,
    500 -> 3
  )

  val houseRiskSurcharge = Vector(
    100 -> 0.7,
    299 -> 1,
    501 -> 2.5
  )

  private def userSurcharge(user: User): Option[BigDecimal] =
    userRiskToSurcharge
      .collectFirst {
        case (limit, surcharge) if user.risk <= limit => BigDecimal(surcharge)
      }

  private def baseProductSurcharge(product: Product): Option[BigDecimal] =
    product match {
      case _: House   => Some(BigDecimal(0.03))
      case _: Banana  => Some(BigDecimal(1.15))
      case _: Bicycle => Some(BigDecimal(0.10))
      case _          => None
    }

  private def houseSurcharge(house: House): Option[BigDecimal] = {
    if (house.value > BigDecimal(10000000)) {
      Some(BigDecimal(1.15))
    } else if (30 > house.size || house.size > 1000) {
      None
    } else {
      houseRiskSurcharge
        .collectFirst {
          case (limit, surcharge) if house.address.locationRisk <= limit =>
            BigDecimal(surcharge)
        }
    }
  }

  private def bananaSurcharge(banana: Banana, user: User): Option[BigDecimal] =
    if ((3 >= banana.blackSpots && banana.blackSpots <= 12) || user.risk > 200) {
      None
    } else {
      Some(BigDecimal(1))
    }

  private def bicycleSurcharge(bike: Bicycle): Option[BigDecimal] = Some(bike.gears * 0.08)

  private def additionalBicycleRules(totalSurcharge: Option[BigDecimal], user: User): Option[BigDecimal] =
    totalSurcharge.filter(_ > 100 && user.risk > 150)

}
