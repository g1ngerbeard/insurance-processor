package coya.models

sealed trait Product {
  def id: Int
  def value: BigDecimal
}

case class House(id: Int, value: BigDecimal, address: Address, size: Int) extends Product
case class Banana(id: Int, value: BigDecimal, blackSpots: Int) extends Product
case class Bicycle(id: Int, value: BigDecimal, gears: Int) extends Product
