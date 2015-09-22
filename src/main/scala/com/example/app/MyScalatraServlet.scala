package com.example.app

import org.scalatra._
import scalate.ScalateSupport
import models._

class MyScalatraServlet extends ScalashopStack {

  get("/") {
    val shop = new Shop(1)
    <html>
      <body>
        <h1>Hello, world!</h1>
        Say <a href="hello-scalate">hello to Scalate</a>.
        {{shop.contents}}
      </body>
    </html>
  }

  get("/hello-scalate"){
    <p>poo</p>
  }
}
