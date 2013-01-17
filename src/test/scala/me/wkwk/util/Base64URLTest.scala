package me.wkwk.util

import org.scalatest.FunSpec

/**
 * Created with IntelliJ IDEA.
 * User: yuichi
 * Date: 2012/11/08
 * Time: 14:26
 * To change this template use File | Settings | File Templates.
 */
class Base64URLTest extends FunSpec{

  describe("Encrypter"){

    it ("input error check"){
      intercept[Exception]{
        Base64URL.decode("%")
      }
    }

    it ("encode / decode test") {
      val input = "abcdefg"
      val output = Base64URL.encode(input)
      println("encode: " + output)
      println("decode: " + Base64URL.decode(output))
      assert(input == Base64URL.decode(Base64URL.encode(input)))
    }

  }

}
