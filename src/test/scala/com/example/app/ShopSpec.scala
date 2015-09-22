package com.example.app

/**
 * Created by maya on 12/09/2015.
 */

import _root_.com.example.app.data.Shop
import org.scalatest.{Matchers, FunSpec}
import org.scalatest.OneInstancePerTest

class ShopSpec extends FunSpec with Matchers with OneInstancePerTest{

  val shop = new Shop()
  val shoes = Map("name"-> "Almond Toe Court Shoes - Patent Black",
    "category" -> "Womens footwear",
    "price" -> 99.0,
    "quantity" -> 5)
  val shirt = Map("name" -> "Fine Stripe Short Sleeve Shirt - Grey",
    "category" -> "Mens casualwear",
    "price" -> 49.99,
    "quantity" -> 9)
  val voucher = Map("name" -> "fiver",
    "price" -> -5.00)

  describe("Scala Shop") {

    it("should be able to have items added") {
      shop.add(shoes)
      shop.contents should contain(shoes)
    }

    it("should be able to have items removed") {
      shop.add(shoes)
      shop.remove(shoes)
      shop.contents should not contain(shoes)
    }

    it("can add up the prices of products inside it") {
      shop.add(shoes)
      shop.add(shirt)
      shop.balance should equal(148.99)
    }

    it("can have a discount voucher applied") {
      shop.add(shoes)
      shop.add(voucher)
      shop.balance should equal(94.00)
    }
  }

}
