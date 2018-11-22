package coya.quotes

import coya.models._
import coya.quotes.CoyaProcessorSpec.rID
import org.scalatest._

import scala.util.Random

class CoyaProcessorSpec extends FlatSpec with Matchers {
  val goodAddress = Address(rID(), 10)
  val badAddress = Address(rID(), 502)

  val user10 = User(rID(), goodAddress, 10)
  val user150 = User(rID(), goodAddress, 150)
  val user201 = User(rID(), goodAddress, 201)
  val user501 = User(rID(), goodAddress, 501)

  val user150address502 = User(rID(), badAddress, 150)

  val goodBanana = Banana(rID(), BigDecimal(15), 2)
  val badBanana = Banana(rID(), BigDecimal(20), 15)

  val funBike = Bicycle(rID(), BigDecimal(1000), 18)

  // todo: use table for all possible combinations
  val coolHouse = House(rID(), BigDecimal(1000000), goodAddress, 40)
  val superExpensiveHouse = House(rID(), BigDecimal(10000001), goodAddress, 40)
  val tooBigHouse = House(rID(), BigDecimal(1000000), goodAddress, 1200)
  val tooSmallHouse = House(rID(), BigDecimal(10000), goodAddress, 30)
  val badHouse = House(rID(), BigDecimal(10000), badAddress, 100)

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
    CoyaProcessor.priceFor(user10, List(badHouse)) shouldEqual None
  }

  /*
   * 10000001 * 0.03 * 1.15 * 0.3 = 103 * 100 ??? = 10350
   */
  "user w/ super expensive house" should "receive an offer" in {
    CoyaProcessor.priceFor(user10, List(superExpensiveHouse)) shouldEqual Some(
      BigDecimal(10350))
  }

  "user w/ too small or too big house" should "be denied" in {
    CoyaProcessor.priceFor(user10, List(tooSmallHouse)) shouldBe None
    CoyaProcessor.priceFor(user10, List(tooBigHouse)) shouldBe None
  }

  /*
   * 1000 * 0.10 * (18 * 0.08) * 0.3 = 43.2
   */
  "user w/ URV 10" should "receive an offer for a bike" in {
    CoyaProcessor.priceFor(user10, List(funBike)) shouldBe Some(BigDecimal(43.2))
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

  /*
   * 15 * 1.15 * 1 = 17.25
   */
  "user w/ URV 150" should "receive an offer for good banana" in {
    CoyaProcessor.priceFor(user150, List(goodBanana)) shouldBe Some(BigDecimal(17.25))
  }

  "user w/ URV 150 and bad banana" should "be denied" in {
    CoyaProcessor.priceFor(user150, List(badBanana)) shouldBe None
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

object CoyaProcessorSpec {
  def rID(): Int = Random.nextInt()
}
