package coya.quotes

import coya.models._

trait Processor {
  def priceFor(u: User, p: Seq[Product]): Option[BigDecimal]
}

object CoyaProcessor extends Processor {

  val initValue: Option[BigDecimal] = Some(BigDecimal(1))

  def priceFor(user: User, products: Seq[Product]): Option[BigDecimal] = {

    val surcharges = userSurcharge(user) +: products.map(p => productSurcharge(user, p).map(_ * p.value))

    surcharges
      .foldLeft(initValue) {
        case (resOpt, surchargeOpt) =>
          for {
            res <- resOpt
            surcharge <- surchargeOpt
          } yield res * surcharge
      }
      .map(_.setScale(0, BigDecimal.RoundingMode.FLOOR))

  }

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
        case (limit, surcharge: Double) if user.risk <= limit =>
          BigDecimal(surcharge)
      }

  def productSurcharge(u: User, p: Product): Option[BigDecimal] =
    p match {
      case h: House   => houseSurcharge(h).map(_ * BigDecimal(0.03))
      case b: Banana  => bananaSurcharge(b, u).map(_ * BigDecimal(1.15))
      case b: Bicycle => bicycleSurcharge(b).map(_ * BigDecimal(0.10)).filter(_ > 100 && u.risk > 150)
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
          case (limit, surcharge: Double)
              if house.address.locationRisk <= limit =>
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

  private def bicycleSurcharge(bike: Bicycle): Option[BigDecimal] =
    Some(bike.gears * 0.08)

}
