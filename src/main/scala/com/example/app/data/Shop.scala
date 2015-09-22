package com.example.app.data

import java.util.concurrent.atomic.AtomicInteger

import com.example.app.models.Shop

import scala.collection.mutable.ArrayBuffer

/**
 * Created by maya on 19/09/2015.
 */
class Shop {

  val idCounter = new AtomicInteger(3)

  var contents = ArrayBuffer [Map[String, Any]]()

  def add(item: Map[String, Any]): Unit = {
    contents  += item
  }

  def remove(item: Map[String, Any]): Unit = {
    contents -= item
  }

  def balance(): Double = {
    var price = 0.0
    for (item <- contents) price += item("price").asInstanceOf[Double]
    price
  }

  def newShop() = Shop(idCounter.incrementAndGet)
}
