package coya.quotes

import java.security.cert.PKIXRevocationChecker.Option

import coya.models._
import org.scalatest._

class CoyaProcessorSpec extends FlatSpec with Matchers {
  val goodAddress = Address(1, 10)
  val badAddress = Address(1, 500)

  val userOne = User(1, goodAddress, 10)
  val userTwo = User(2, badAddress, 150)

  val goodBanana = Banana(12, BigDecimal(15), 2)

  val funBike = Bicycle(1, BigDecimal(1000), 18)
  val coolHouse = House(2, BigDecimal(1000000), goodAddress, 40)

  /*
   1,000,000 * // house value
   0.03 * // house base premium value
   0.7 * // house risk surcharge
   0.3 // user risk surcharge
   = 630 € per year
   */
  "userOne with coolHouse" should "receive a good offer" in {
    CoyaProcessor.priceFor(userOne, List(coolHouse)) shouldEqual Some(BigDecimal(630))
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
  "userTwo with funBike" should "be denied" in {
    CoyaProcessor.priceFor(userTwo, List(funBike)) shouldEqual None
  }

  "insurance offer" should "be denied for all products if one rule fail" in {
    CoyaProcessor.priceFor(userTwo, List(funBike, coolHouse)) shouldBe None
  }

  it should "be approved for multiple products if all rules pass" in {
    CoyaProcessor.priceFor(userTwo, List(funBike, goodBanana)) shouldNot be (None)
  }

}
