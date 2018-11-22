package coya.quotes

import coya.models._
import org.scalatest._

class CoyaProcessorSpec extends FlatSpec with Matchers {
  val goodAddress = Address(1, 10)
  val badAddress = Address(1, 502)

  val user10 = User(1, goodAddress, 10)
  val user150 = User(2, goodAddress, 150)
  val user201 = User(3, goodAddress, 201)
  val user501 = User(4, goodAddress, 501)

  val user150address502 = User(21, badAddress, 150)

  val goodBanana = Banana(12, BigDecimal(15), 2)

  val funBike = Bicycle(1, BigDecimal(1000), 18)

  // todo: use table for all possible combinations
  val coolHouse = House(2, BigDecimal(1000000), goodAddress, 40)
  val superExpensiveHouse = House(2, BigDecimal(10000001), goodAddress, 40)
  val tooBigHouse = House(3, BigDecimal(1000000), goodAddress, 1200)
  val tooSmallHouse = House(4, BigDecimal(10000), goodAddress, 30)
  val badHouse  = House(3, BigDecimal(10000), badAddress, 100)

  /*
   1000000 * 0.03 * 0.7 * 0.3 = 6.3 * 100 ??? = 630

   1,000,000 * // house value
   0.03 * // house base premium value
   0.7 * // house risk surcharge
   0.3 // user risk surcharge
   = 630 € per year
   */
  "user w/ URV 10 and house w/ URV 10" should "receive a good offer" in {
    CoyaProcessor.priceFor(user10, List(coolHouse)) shouldEqual Some(BigDecimal(630))
  }

  "any user and house w/ URV > 501" should "be denied" in {
    CoyaProcessor.priceFor(user10, List(badHouse)) shouldEqual Some(BigDecimal(630))
  }

  /*
    10000001 * 0.03 * 1.15 * 0.3 = 103 * 100 ??? = 10300
  */
  "user w/ super expensive house" should "receive an offer" in {
    CoyaProcessor.priceFor(user10, List(superExpensiveHouse)) shouldEqual Some(BigDecimal(10300))
  }

  "user w/ too small or too bif house" should "be denied" in {
    CoyaProcessor.priceFor(user10, List(tooSmallHouse)) shouldBe None
    CoyaProcessor.priceFor(user10, List(tooBigHouse)) shouldBe None
  }

  /*
   1000 * // bike value
   0.10 * // bike base premium value
   (18 * 0.08) * // gears surcharge
   1 // user risk surcharge
   = 144 € per year

   Given that userTwo has a risk value of more than 150 and the total
   premium is bigger than 100 €, we won't offer him insurance.
   */
  "user w/ URV 150 with bike w/ premium > 100" should "be denied" in {
    CoyaProcessor.priceFor(user201, List(funBike)) shouldBe None
  }

  "user w/ URV > 500" should "be denied" in {
    CoyaProcessor.priceFor(user501, List(coolHouse)) shouldBe None
  }

  "insurance offer" should "be denied for all products if one rule fail" in {
    CoyaProcessor.priceFor(user201, List(funBike, coolHouse)) shouldBe None
  }

  it should "be approved for multiple products if all rules pass" in {
    CoyaProcessor.priceFor(user201, List(funBike, goodBanana)) shouldNot be(None)
  }

}
